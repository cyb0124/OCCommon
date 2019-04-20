local round = function(num)
  return math.floor(num + 0.5)
end

local Dispatch = (function(signalHandler)
  local Dispatch = {}
  local evQueue = {}
  local alarms = {}

  Dispatch.queue = function(x)
    table.insert(evQueue, x)
  end

  Dispatch.setAlarm = function(t1, f)
    local alarm = {t1 = t1, f = f}
    alarms[alarm] = true
    return function()
      alarm.f = nil
    end
  end

  Dispatch.setTimer = function(td, f)
    return Dispatch.setAlarm(computer.uptime() + td, f)
  end

  Dispatch.run = function(x)
    while true do
      local timeout = math.huge
      local evQueueNow = evQueue
      evQueue = {}
      local alarmsNow = alarms
      alarms = {}
      for i = 1, #evQueueNow do
        evQueueNow[i]()
        timeout = 0
      end
      local now = computer.uptime()
      for alarm, _ in pairs(alarmsNow) do
        if alarm.f then
          if alarm.t1 <= now then
            alarm.f()
            timeout = 0
          else
            alarms[alarm] = true
            timeout = math.min(now - alarm.t1, timeout)
          end
        end
      end
      local result = {computer.pullSignal(timeout)}
      if result[1] then
        signalHandler(table.unpack(result))
      end
    end
  end

  return Dispatch
end)

local serpent = (function()
  local n, v = "serpent", "0.302" -- (C) 2012-18 Paul Kulchenko; MIT License
  local c, d = "Paul Kulchenko", "Lua serializer and pretty printer"
  local snum = {[tostring(1/0)]='1/0 --[[math.huge]]',[tostring(-1/0)]='-1/0 --[[-math.huge]]',[tostring(0/0)]='0/0'}
  local badtype = {thread = true, userdata = true, cdata = true}
  local getmetatable = debug and debug.getmetatable or getmetatable
  local pairs = function(t) return next, t end -- avoid using __pairs in Lua 5.2+
  local keyword, globals, G = {}, {}, (_G or _ENV)
  for _,k in ipairs({'and', 'break', 'do', 'else', 'elseif', 'end', 'false',
    'for', 'function', 'goto', 'if', 'in', 'local', 'nil', 'not', 'or', 'repeat',
    'return', 'then', 'true', 'until', 'while'}) do keyword[k] = true end
  for k,v in pairs(G) do globals[v] = k end -- build func to name mapping
  for _,g in ipairs({'coroutine', 'debug', 'io', 'math', 'string', 'table', 'os'}) do
    for k,v in pairs(type(G[g]) == 'table' and G[g] or {}) do globals[v] = g..'.'..k end end

  local function s(t, opts)
    local name, indent, fatal, maxnum = opts.name, opts.indent, opts.fatal, opts.maxnum
    local sparse, custom, huge = opts.sparse, opts.custom, not opts.nohuge
    local space, maxl = (opts.compact and '' or ' '), (opts.maxlevel or math.huge)
    local maxlen, metatostring = tonumber(opts.maxlength), opts.metatostring
    local iname, comm = '_'..(name or ''), opts.comment and (tonumber(opts.comment) or math.huge)
    local numformat = opts.numformat or "%.17g"
    local seen, sref, syms, symn = {}, {'local '..iname..'={}'}, {}, 0
    local function gensym(val) return '_'..(tostring(tostring(val)):gsub("[^%w]",""):gsub("(%d%w+)",
      -- tostring(val) is needed because __tostring may return a non-string value
      function(s) if not syms[s] then symn = symn+1; syms[s] = symn end return tostring(syms[s]) end)) end
    local function safestr(s) return type(s) == "number" and tostring(huge and snum[tostring(s)] or numformat:format(s))
      or type(s) ~= "string" and tostring(s) -- escape NEWLINE/010 and EOF/026
      or ("%q"):format(s):gsub("\010","n"):gsub("\026","\\026") end
    local function comment(s,l) return comm and (l or 0) < comm and ' --[['..select(2, pcall(tostring, s))..']]' or '' end
    local function globerr(s,l) return globals[s] and globals[s]..comment(s,l) or not fatal
      and safestr(select(2, pcall(tostring, s))) or error("Can't serialize "..tostring(s)) end
    local function safename(path, name) -- generates foo.bar, foo[3], or foo['b a r']
      local n = name == nil and '' or name
      local plain = type(n) == "string" and n:match("^[%l%u_][%w_]*$") and not keyword[n]
      local safe = plain and n or '['..safestr(n)..']'
      return (path or '')..(plain and path and '.' or '')..safe, safe end
    local alphanumsort = type(opts.sortkeys) == 'function' and opts.sortkeys or function(k, o, n) -- k=keys, o=originaltable, n=padding
      local maxn, to = tonumber(n) or 12, {number = 'a', string = 'b'}
      local function padnum(d) return ("%0"..tostring(maxn).."d"):format(tonumber(d)) end
      table.sort(k, function(a,b)
        -- sort numeric keys first: k[key] is not nil for numerical keys
        return (k[a] ~= nil and 0 or to[type(a)] or 'z')..(tostring(a):gsub("%d+",padnum))
             < (k[b] ~= nil and 0 or to[type(b)] or 'z')..(tostring(b):gsub("%d+",padnum)) end) end
    local function val2str(t, name, indent, insref, path, plainindex, level)
      local ttype, level, mt = type(t), (level or 0), getmetatable(t)
      local spath, sname = safename(path, name)
      local tag = plainindex and
        ((type(name) == "number") and '' or name..space..'='..space) or
        (name ~= nil and sname..space..'='..space or '')
      if seen[t] then -- already seen this element
        sref[#sref+1] = spath..space..'='..space..seen[t]
        return tag..'nil'..comment('ref', level) end
      -- protect from those cases where __tostring may fail
      if type(mt) == 'table' and metatostring ~= false then
        local to, tr = pcall(function() return mt.__tostring(t) end)
        local so, sr = pcall(function() return mt.__serialize(t) end)
        if (to or so) then -- knows how to serialize itself
          seen[t] = insref or spath
          t = so and sr or tr
          ttype = type(t)
        end -- new value falls through to be serialized
      end
      if ttype == "table" then
        if level >= maxl then return tag..'{}'..comment('maxlvl', level) end
        seen[t] = insref or spath
        if next(t) == nil then return tag..'{}'..comment(t, level) end -- table empty
        if maxlen and maxlen < 0 then return tag..'{}'..comment('maxlen', level) end
        local maxn, o, out = math.min(#t, maxnum or #t), {}, {}
        for key = 1, maxn do o[key] = key end
        if not maxnum or #o < maxnum then
          local n = #o -- n = n + 1; o[n] is much faster than o[#o+1] on large tables
          for key in pairs(t) do if o[key] ~= key then n = n + 1; o[n] = key end end end
        if maxnum and #o > maxnum then o[maxnum+1] = nil end
        if opts.sortkeys and #o > maxn then alphanumsort(o, t, opts.sortkeys) end
        local sparse = sparse and #o > maxn -- disable sparsness if only numeric keys (shorter output)
        for n, key in ipairs(o) do
          local value, ktype, plainindex = t[key], type(key), n <= maxn and not sparse
          if opts.valignore and opts.valignore[value] -- skip ignored values; do nothing
          or opts.keyallow and not opts.keyallow[key]
          or opts.keyignore and opts.keyignore[key]
          or opts.valtypeignore and opts.valtypeignore[type(value)] -- skipping ignored value types
          or sparse and value == nil then -- skipping nils; do nothing
          elseif ktype == 'table' or ktype == 'function' or badtype[ktype] then
            if not seen[key] and not globals[key] then
              sref[#sref+1] = 'placeholder'
              local sname = safename(iname, gensym(key)) -- iname is table for local variables
              sref[#sref] = val2str(key,sname,indent,sname,iname,true) end
            sref[#sref+1] = 'placeholder'
            local path = seen[t]..'['..tostring(seen[key] or globals[key] or gensym(key))..']'
            sref[#sref] = path..space..'='..space..tostring(seen[value] or val2str(value,nil,indent,path))
          else
            out[#out+1] = val2str(value,key,indent,nil,seen[t],plainindex,level+1)
            if maxlen then
              maxlen = maxlen - #out[#out]
              if maxlen < 0 then break end
            end
          end
        end
        local prefix = string.rep(indent or '', level)
        local head = indent and '{\n'..prefix..indent or '{'
        local body = table.concat(out, ','..(indent and '\n'..prefix..indent or space))
        local tail = indent and "\n"..prefix..'}' or '}'
        return (custom and custom(tag,head,body,tail,level) or tag..head..body..tail)..comment(t, level)
      elseif badtype[ttype] then
        seen[t] = insref or spath
        return tag..globerr(t, level)
      elseif ttype == 'function' then
        seen[t] = insref or spath
        if opts.nocode then return tag.."function() --[[..skipped..]] end"..comment(t, level) end
        local ok, res = pcall(string.dump, t)
        local func = ok and "((loadstring or load)("..safestr(res)..",'@serialized'))"..comment(t, level)
        return tag..(func or globerr(t, level))
      else return tag..safestr(t) end -- handle all other types
    end
    local sepr = indent and "\n" or ";"..space
    local body = val2str(t, name, indent) -- this call also populates sref
    local tail = #sref>1 and table.concat(sref, sepr)..sepr or ''
    local warn = opts.comment and #sref>1 and space.."--[[incomplete output with shared/self-references skipped]]" or ''
    return not name and body..warn or "do local "..body..sepr..tail.."return "..name..sepr.."end"
  end

  local function deserialize(data, opts)
    local env = (opts and opts.safe == false) and G
      or setmetatable({}, {
          __index = function(t,k) return t end,
          __call = function(t,...) error("cannot call functions") end
        })
    local f, res = (loadstring or load)('return '..data, nil, nil, env)
    if not f then f, res = (loadstring or load)(data, nil, nil, env) end
    if not f then return f, res end
    if setfenv then setfenv(f, env) end
    return pcall(f)
  end

  local function merge(a, b) if b then for k,v in pairs(b) do a[k] = v end end; return a; end
  return { _NAME = n, _COPYRIGHT = c, _DESCRIPTION = d, _VERSION = v, serialize = s,
    load = deserialize,
    dump = function(a, opts) return s(a, merge({name = '_', compact = true, sparse = true}, opts)) end,
    line = function(a, opts) return s(a, merge({sortkeys = true, comment = true}, opts)) end,
    block = function(a, opts) return s(a, merge({indent = '  ', sortkeys = true, comment = true}, opts)) end }
end)()

-- Assuming: serpent

local lpsDump = function(x)
  x = serpent.dump(x)
  local lengthString = tostring(#x)
  return string.char(#lengthString) .. lengthString .. x
end

local LpsParser = function(callback)
  local LpsParser = {}
  local state, StateInit, StateLength, StateContent
  LpsParser.push = function(x)
    if #x > 0 then
      state.push(x)
    end
  end
  local changeState = function(newState)
    state = newState
  end

  StateInit = function()
    local state = {}
    state.push = function(x)
      changeState(StateLength(string.byte(string.sub(x, 1, 1))))
      LpsParser.push(string.sub(x, 2))
    end
    return state
  end

  StateLength = function(lengthOfLength)
    local state = {}
    local buffer = ""
    state.push = function(x)
      buffer = buffer .. x
      if #buffer >= lengthOfLength then
        changeState(StateContent(tonumber(string.sub(buffer, 1, lengthOfLength))))
        LpsParser.push(string.sub(buffer, lengthOfLength + 1))
      end
    end
    return state
  end

  StateContent = function(length)
    local state = {}
    local buffer = ""
    state.push = function(x)
      buffer = buffer .. x
      if #buffer >= length then
        callback(load(string.sub(buffer, 1, length))())
        changeState(StateInit())
        LpsParser.push(string.sub(buffer, length + 1))
      end
    end
    return state
  end

  changeState(StateInit())

  return LpsParser
end

local TIMEOUT = 3
local NAGLING = 0
local BUFFER  = 4096

local Rx = function(dispatch, updateAckWnd, notify)
  local Rx = {}
  local seq = 0
  local buf = ""
  Rx.push = function(data)
    if #data.dat > 0 then
      local left = data.seq - seq
      local right = math.min(left + #data.dat, BUFFER)
      if left >= 0 and left <= #buf and right > #buf then
        buf = string.sub(buf, 1, left) .. string.sub(data.dat, 1, right - left)
        notify()
      end
      updateAckWnd(seq, BUFFER - #buf)
    end
  end
  Rx.size = function()
    return #buf
  end
  Rx.read = function(n)
    local toRead = math.min(n, #buf)
    if toRead > 0 then
      local result = string.sub(buf, 1, toRead)
      buf = string.sub(buf, toRead + 1)
      seq = seq + toRead
      updateAckWnd(seq, BUFFER - #buf)
      return result
    else
      return ""
    end
  end
  return Rx
end

local Tx = function(dispatch, push, done)
  local Tx = {}
  local seq = 0
  local wnd = BUFFER
  local unack = 0
  local buf = ""
  local myAck = 0
  local myWnd = BUFFER
  local nagleTimer, timeoutTimer
  local transmit, timeout
  transmit = function()
    if not nagleTimer then
      local newTimer
      newTimer = dispatch.setTimer(NAGLING, function()
        if nagleTimer == newTimer then
          local len = math.min(#buf, wnd) - unack
          push({
            seq = seq + unack,
            dat = string.sub(buf, unack + 1, unack + len),
            ack = myAck,
            wnd = myWnd
          })
          unack = unack + len
          nagleTimer = nil
          if len > 0 then
            timeout()
          end
        end
      end)
      nagleTimer = newTimer
    end
  end
  timeout = function()
    if timeoutTimer then
      timeoutTimer()
    end
    local newTimer
    newTimer = dispatch.setTimer(TIMEOUT, function()
      if timeoutTimer == newTimer then
        unack = 0
        transmit()
        timeoutTimer = nil
      end
    end)
    timeoutTimer = newTimer
  end
  Tx.push = function(data)
    wnd = data.wnd
    local maxAck = seq + math.min(wnd, unack)
    if data.ack > seq and data.ack <= maxAck then
      local increment = data.ack - seq
      seq = seq + increment
      unack = unack - increment
      buf = string.sub(buf, increment + 1)
      if #buf <= 0 then
        if timeoutTimer then
          timeoutTimer()
          timeoutTimer = nil
        end
        done()
      else
        timeout()
      end
    end
    if #buf > unack and wnd >= unack then
      transmit()
    end
  end
  Tx.updateAckWnd = function(ack, wnd)
    myAck = ack
    myWnd = wnd
    transmit()
  end
  Tx.send = function(data)
    if #data > 0 then
      buf = buf .. data
      transmit()
    end
  end
  Tx.close = function()
    if nagleTimer then
      nagleTimer()
      nagleTimer = nil
    end
    if timeoutTimer then
      timeoutTimer()
      timeoutTimer = nil
    end
  end
  return Tx
end

local HEARTBEAT_INTERVAL = 10
local HEARTBEAT_TIMEOUT = 15
local CONNECT_TIMEOUT = 3

-- Assuming: lpsDump, LpsParser, Rx, Tx
-- callbacks: push, connected, error, packet

local Peer = function(dispatch, callbacks, passive, connTimeoutOverride)
  if not connTimeoutOverride then connTimeoutOverride = CONNECT_TIMEOUT end
  local Peer = {}
  local state
  local changeState = function(newState)
    if state then state.close() end
    state = newState
  end
  Peer.close = function()
    changeState(nil)
  end
  Peer.send = function(data)
    state.send(data)
  end
  Peer.push = function(data)
    state.push(data)
  end
  Peer.activeClose = function()
    callbacks.push{rst = true}
    Peer.close()
  end
  local StateConnect, StateRun
  StateConnect = function()
    local state = {}
    local alive = true
    local timer
    state.push = function(data)
      if data.rst then
        dispatch.queue(function()
          if alive then
            Peer.close()
            callbacks.error()
          end
        end)
      else
        changeState(StateRun())
      end
    end
    state.close = function()
      if alive then
        if timer then timer() end
        alive = false
      end
    end
    dispatch.queue(function()
      if alive then
        callbacks.push{}
        timer = dispatch.setTimer(connTimeoutOverride, function()
          if alive then
            Peer.close()
            callbacks.error()
          end
        end)
      end
    end)
    return state
  end
  StateRun = function()
    local state = {}
    local alive = true
    local rx, tx, heartbeatTimer, watchdogTimer, resetHeartbeat, resetWatchdog
    local lpsParser
    rx = Rx(dispatch,
      function(ack, wnd)
        if alive then
          tx.updateAckWnd(ack, wnd)
        end
      end,
      function()
        if alive and rx.size() > 0 then
          resetWatchdog()
          lpsParser.push(rx.read(rx.size()))
        end
      end)
    tx = Tx(dispatch,
      function(data)
        if alive then
          callbacks.push(data)
        end
      end,
      function() end
    )
    state.close = function()
      tx.close()
      if heartbeatTimer then heartbeatTimer() end
      if watchdogTimer then watchdogTimer() end
      alive = false
    end
    state.send = function(packet)
      if alive then
        tx.send(lpsDump(packet))
        resetHeartbeat()
      end
    end
    state.push = function(data)
      if alive then
        if data.seq then
          rx.push(data)
          tx.push(data)
        elseif data.rst then
          Peer.close()
          callbacks.error()
        end
      end
    end
    lpsParser = LpsParser(function(packet)
      if not packet.heartbeat then
        dispatch.queue(function()
          if alive then
            callbacks.packet(packet)
          end
        end)
      end
    end)
    resetHeartbeat = function()
      if heartbeatTimer then heartbeatTimer() end
      local newTimer
      newTimer = dispatch.setTimer(HEARTBEAT_INTERVAL, function()
        if alive and heartbeatTimer == newTimer then
          Peer.send{heartbeat = true}
        end
      end)
      heartbeatTimer = newTimer
    end
    resetWatchdog = function()
      if watchdogTimer then watchdogTimer() end
      local newTimer
      newTimer = dispatch.setTimer(HEARTBEAT_TIMEOUT, function()
        if alive and watchdogTimer == newTimer then
          Peer.close()
          callbacks.error()
        end
      end)
      watchdogTimer = newTimer
    end
    if passive then
      dispatch.queue(function()
        if alive then
          callbacks.push{}
        end
      end)
    end
    dispatch.queue(function()
      if alive then
        resetHeartbeat()
        resetWatchdog()
        callbacks.connected()
      end
    end)
    return state
  end
  if passive then
    changeState(StateRun())
  else
    changeState(StateConnect())
  end
  return Peer
end

local resolve = function(short)
  for addr in component.list(short) do
    return addr
  end
  for addr in component.list() do
    if string.lower(string.sub(addr, 1, #short)) == string.lower(short) then
      return addr
    end
  end
end

local sides = {
  bottom = 0, down  = 0, yn = 0,
  top    = 1, up    = 1, yp = 1,
  back   = 2, north = 2, zn = 2,
  front  = 3, south = 3, zp = 3,
  right  = 4, west  = 4, xn = 4,
  left   = 5, east  = 5, xp = 5
}

local cps = function(f)
  return function(c, ...)
    c(f(...))
  end
end

local cpsThen = function(f, g)
  return function(c)
    f(function(...) g(c, ...) end)
  end
end

local cpsChain = function(f, ...)
  local fs = {...}
  for i = 1, #fs do f = cpsThen(f, fs[i]) end
  return f
end

local cpsAll = function(...)
  local fs = {...}
  return function(done, ...)
    if #fs <= 0 then done{} return end
    local nFinished, args, results = 0, {...}, {}
    for i, f in ipairs(fs) do
      f(function(...)
        results[i] = {...}
        nFinished = nFinished + 1
        if nFinished >= #fs then
          done(results)
        end
      end, table.unpack(args))
    end
  end
end

local cpsWrap = function(f)
  return function(c, ...)
    local co, resume = coroutine.create(f)
    resume = function(...)
      local result = {coroutine.resume(co, ...)}
      if not result[1] then
        error(result[2])
      else
        if result[2] == "done" then
          c(select(3, table.unpack(result)))
        elseif result[2] == "then" then
          result[3](resume, select(4, table.unpack(result)))
        else
          error "invalid yield"
        end
      end
    end
    resume(...)
  end
end

local WrappedMutex = function(dispatch)
  local result, waitQueue, locked = {}, {}
  result.lock = function()
    coroutine.yield("then", function(cont)
      if locked then
        table.insert(waitQueue, cont)
      else
        locked = true
        cont()
      end
    end)
  end
  result.unlock = function()
    if #waitQueue > 0 then
      dispatch.queue(table.remove(waitQueue, 1))
    else
      locked = false
    end
  end
  return result
end

-- Config

local inv = component.proxy(resolve("transposer"))
local modem = component.proxy(resolve("modem"))
local channel = 1
local hostname = "factory"
local sideDrawer = sides.north
local sideTrashCan = sides.down
local sideBus = sides.south
local gpus = {
  {proxy = component.proxy(resolve("gpu")), resolution = {80, 25}}
}

-- GPU

local print = function(x, color, beep)
  for i = 1, #gpus do
    if not color then
      color = 0xFFFFFF
    end
    local resolution = gpus[i].resolution
    gpus[i].proxy.copy(1, 1, resolution[1], resolution[2], 0, -1)
    gpus[i].proxy.fill(1, resolution[2], resolution[1], resolution[2], ' ')
    gpus[i].proxy.setForeground(color)
    gpus[i].proxy.set(1, resolution[2], x)
  end
  if beep then computer.beep(beep) end
end

-- Networking

if modem.setStrength then modem.setStrength(math.huge) end
modem.open(channel)
local iClients, oClients, IClient = {}, {}
local dispatch = Dispatch(function(kind, ...)
  if kind == "modem_message" then
    local data, remoteUUID, from, to = load(select(5, ...))(), (select(2, ...)), select(6, ...)
    if to == hostname then
      local client = iClients[from]
      if client then
        client.remoteUUID = remoteUUID
        client.peer.push(data)
      elseif data.seq then
        modem.send(remoteUUID, channel, serpent.dump{rst = true}, hostname, from)
      elseif not data.rst then
        IClient(from, remoteUUID)
      end
    elseif string.match(to, "[^:]*") == hostname then
      local client = oClients[to]
      if client and from == client.remoteAddr then
        client.remoteUUID = remoteUUID
        client.peer.push(data)
      else
        modem.send(remoteUUID, channel, serpent.dump{rst = true}, to, from)
      end
    end
  end
end)

local randomSourcePort = function()
  local chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  local result = ""
  for i = 1, 8 do
    local index = math.random(#chars)
    result = result .. string.sub(chars, index, index)
  end
  return result
end

local HandlerCPS, cpsHandleInit = function(iClient, wrapped, cont)
  local result, alive, recvBuffer, recvCont = {}, true, {}
  result.close = function()
    alive = false
    if recvCont then
      recvCont()
    end
  end
  result.packet = function(p)
    if alive then
      if recvCont then
        local cont = recvCont
        recvCont = nil
        cont(p)
      else
        table.insert(recvBuffer, p)
      end
    end
  end
  local callbacks = {
    send = function(cont, p)
      if alive then
        iClient.peer.send(p)
        cont(true)
      else
        cont(false)
      end
    end,
    recv = function(cont)
      if alive then
        if #recvBuffer > 0 then
          local p = table.remove(recvBuffer, 1)
          if #recvBuffer <= 0 then recvBuffer = {} end
          cont(p)
        else
          recvCont = cont
        end
      else
        cont()
      end
    end,
    close = function(cont)
      if alive then
        iClient.close()
      end
      cont()
    end,
    getFrom = function(cont)
      cont(iClient.from)
    end
  }
  local oldFinish = callbacks.finish
  if wrapped then
    local oldCallbacks = callbacks
    callbacks = setmetatable({}, {__index = function(_, key)
      return function(...)
        return coroutine.yield("then", oldCallbacks[key], ...)
      end
    end})
    cont = cpsWrap(cont)
  end
  dispatch.queue(function() cont(function()
    if alive then
      iClient.close()
    end
  end, callbacks) end)
  return result
end

IClient = function(from, initRemoteUUID)
  print("[+]" .. from, 0x00ff00)
  local result = {remoteUUID = initRemoteUUID, from = from}
  result.close = function()
    result.peer.activeClose()
    result.changeHandler(nil)
    iClients[from] = nil
    print("[-]" .. from, 0x00ff00)
  end
  result.changeHandler = function(newHandler)
    if result.handler then result.handler.close() end
    result.handler = newHandler
  end
  result.peer = Peer(dispatch, {
    push = function(x)
      modem.send(result.remoteUUID, channel, serpent.dump(x), hostname, from)
    end,
    connected = function()
      result.changeHandler(HandlerCPS(result, true, cpsHandleInit))
    end,
    error = function()
      result.changeHandler(nil)
      iClients[from] = nil
      print("[-]" .. from, 0xff0000)
    end,
    packet = function(p)
      if result.handler then
        result.handler.packet(p)
      end
    end
  }, true)
  iClients[from] = result
end

local OClient = function(to, connTimeoutOverride)
  local result = {}
  result.close = function()
    result.peer.activeClose()
    oClients[result.localAddr] = nil
  end
  result.remoteAddr = to
  result.localAddr = hostname .. ":" .. randomSourcePort()
  result.peer = Peer(dispatch, {
    push = function(x)
      if result.remoteUUID then
        modem.send(result.remoteUUID, channel, serpent.dump(x), result.localAddr, result.remoteAddr)
      else
        modem.broadcast(channel, serpent.dump(x), result.localAddr, result.remoteAddr)
      end
    end,
    connected = function()
      result.connected()
    end,
    error = function()
      result.error()
      oClients[result.localAddr] = nil
    end,
    packet = function(p)
      result.packet(p)
    end
  }, false, connTimeoutOverride)
  oClients[result.localAddr] = result
  return result
end

local cpsConnect = function(cont, to, connTimeoutOverride)
  local result, client, recvBuffer, callbacks, recvCont = {}, OClient(to, connTimeoutOverride)
  client.connected = function()
    if client then
      recvBuffer = {}
      cont(callbacks)
    end
  end
  client.error = function()
    if client then
      client = nil
      if not recvBuffer then
        computer.beep(880)
        cont()
      elseif recvCont then
        recvCont()
      end
    end
  end
  client.packet = function(p)
    if client then
      if recvCont then
        local cont = recvCont
        recvCont = nil
        cont(p)
      else
        table.insert(recvBuffer, p)
      end
    end
  end
  callbacks = {
    close = function(cont)
      if client then
        client.close()
        client = nil
      end
      cont()
    end,
    send = function(cont, p)
      if client then
        client.peer.send(p)
        cont(true)
      else
        cont(false)
      end
    end,
    recv = function(cont)
      if client then
        if #recvBuffer > 0 then
          local p = table.remove(recvBuffer, 1)
          if #recvBuffer <= 0 then recvBuffer = {} end
          cont(p)
        else
          recvCont = cont
        end
      else
        cont()
      end
    end
  }
end

local wrappedConnUtils = function(to, connTimeoutOverride)
  local result, conn = {}
  result.connect = function()
    if not conn then
      conn = coroutine.yield("then", cpsConnect, to, connTimeoutOverride)
      if conn then
        return "connected"
      else
        return "failed"
      end
    else
      return "already"
    end
  end
  result.send = function(p)
    if not conn then return end
    local result = coroutine.yield("then", conn.send, p)
    if not result then conn = nil end
    return result
  end
  result.recv = function()
    if not conn then return end
    local result = coroutine.yield("then", conn.recv)
    if not result then conn = nil end
    return result
  end
  result.close = function()
    if not conn then return end
    local result = coroutine.yield("then", conn.close)
    conn = nil
  end
  return result
end

-- Bus

local busAllocations, busWaitQueue, insertItem, destroyItem, shouldDestroy = {}, {}
local busUpdate = function()
  local stacks = inv.getAllStacks(sideBus)
  local freeSlots, recheck = {}
  for i = 1, stacks.count() do
    local item = stacks()
    if not busAllocations[i] then
      if not item.name then item.size = 0 end
      if item.size > 0 then
        if shouldDestroy(item) then
          print(item.label .. "*" .. math.floor(item.size), 0xff00ff)
          destroyItem(i, item.size)
        else
          print(item.label .. "*" .. math.floor(item.size), 0xffa500)
          insertItem(i, item.size)
        end
        recheck = true
      else
        table.insert(freeSlots, i)
      end
    end
  end
  local newBusWaitQueue = {}
  for _, task in ipairs(busWaitQueue) do
    if task.n <= #freeSlots or task.allowPartial and #freeSlots > 0 then
      local allocatedSlots = {}
      for i = 1, math.min(task.n, #freeSlots) do
        local freeSlot = table.remove(freeSlots)
        busAllocations[freeSlot] = true
        allocatedSlots[i] = freeSlot
      end
      dispatch.queue(function() task.cont(allocatedSlots) end)
    else
      table.insert(newBusWaitQueue, task)
    end
  end
  busWaitQueue = newBusWaitQueue
  if recheck then dispatch.queue(busUpdate) end
end
local busAllocateN = function(cont, n, allowPartial)
  table.insert(busWaitQueue, {n = n, cont = cont, allowPartial = allowPartial})
  busUpdate()
end
local busFreeN = function(slots)
  for _, slot in ipairs(slots) do busAllocations[slot] = nil end
  busUpdate()
end
local busAllocate = function(cont)
  busAllocateN(function(slots) cont(slots[1]) end, 1)
end
local busFree = function(slot)
  busFreeN{slot}
end

-- Inventory utils

local forEachSlot = function(inv, side, f)
  local stacks = inv.getAllStacks(side)
  local count = stacks.count()
  for slot = 1, count do
    local item = stacks()
    if item.name and item.size > 0 then
      item.size = math.floor(item.size)
      f(item, slot)
    end
  end
  return count
end

-- Item management

local labelMap

local getItem = function(x)
  if type(x) == "string" then return labelMap[x] else return x end
end

local getAvail = function(x, allowBackup)
  x = getItem(x)
  if not x then return 0 end
  if x.backup and not allowBackup then return x.size - x.backup end
  return x.size
end

local getListAvail = function(itemList)
  local result = math.huge
  for _, v in pairs(itemList) do
    result = math.min(result, math.floor(getAvail(v.item, v.allowBackup) / v.size))
  end
  return result
end

local rankRecipes = function(recipes, adjust)
  for _, recipe in pairs(recipes) do
    if recipe.is then recipe.ia = getListAvail(recipe.is)
    else recipe.ia = getAvail(recipe.i, recipe.allowBackup) end
    recipe.oa = getAvail(recipe.o, true)
    if recipe.ia <= 0 then
      recipe.demand = -math.huge
    else
      recipe.demand = 1 - recipe.oa / recipe.size
    end
    if adjust then adjust(recipe) end
  end
  table.sort(recipes, function(x, y)
    return x.demand > y.demand
  end)
end

local backupItem = function(x, qty)
  x = getItem(x)
  qty = math.min(getAvail(x), qty)
  if qty <= 0 then return 0 end
  if x.backup then x.backup = x.backup + qty
  else x.backup = qty end
  return qty
end

local reserveItem = function(x, qty, allowBackup)
  x = getItem(x)
  qty = math.min(getAvail(x, allowBackup), qty)
  if qty <= 0 then return 0 end
  x.size = x.size - qty
  if x.backup and x.backup < x.size then x.backup = x.size end
  return qty
end

local extractItem = function(x, qty, reason, allowBackup)
  local result = {
    size = 0,
    extract = function(slot) end
  }
  x = getItem(x)
  result.size = reserveItem(x, qty, allowBackup)
  if result.size > 0 then
    if reason then reason = reason .. ": "
    else reason = "" end
    print(reason .. x.label .. "*" .. math.floor(result.size), 0x55abec)
    result.extract = function(slot)
      inv.transferItem(sideDrawer, sideBus, result.size, x.slot, slot)
    end
  end
  return result
end

insertItem = function(slot, size)
  inv.transferItem(sideBus, sideDrawer, size, slot)
end

destroyItem = function(slot, size)
  inv.transferItem(sideBus, sideTrashCan, size, slot)
end

-- Handlers

cpsHandleInit = function(conn)
  while true do
    local p = conn.recv()
    if not p then return "done" end
    -- dispatch here
    conn.close()
  end
end

-- Cycle

local cycleCounter, lastCycleTime = 1, math.huge

local cycleStart = function(cont)
  print("Cycle #" .. cycleCounter .. ", lastCycleTime=" .. round(lastCycleTime * 100) / 100)
  labelMap = {}
  forEachSlot(inv, sideDrawer, function(item, slot)
    item.slot = slot
    labelMap[item.label] = item
  end)
  backupItem("Soybean",    64)
  backupItem("Strawberry", 64)
  cont()
end

local junkMap = {["Bow"]=1, ["Chain Boots"]=1, ["Golden Boots"]=1}
shouldDestroy = function(item)
  return junkMap[item.label]
end

local cycleEnd = function(cont)
  labelMap = nil
  busUpdate()
  cycleCounter = cycleCounter + 1
  cont()
end

local runProxyGroup = function(conn, actions)
  local ps, nonEmpty = {}
  for k, v in ipairs(actions) do ps[k] = v.p; nonEmpty = true end
  if not nonEmpty then return true end
  conn.connect()
  conn.send{ps = ps}
  ps = conn.recv()
  if not ps then return end
  for k, v in ipairs(actions) do if v.f then v.f(ps[k]) end end
  return true
end

-- fn: (plan, preActions) -> ()
-- plan: {(slot, postActions) -> (), ...}
local proxyCycleTemplate = function(host, fn)
  local conn = wrappedConnUtils(host)
  return cpsWrap(function()
    local plan, preActions = {}, {}
    fn(plan, preActions)
    if not runProxyGroup(conn, preActions) then print(host .. ": preActions failed", 0xff0000) end
    if #plan <= 0 then return "done" end
    local slots = coroutine.yield("then", busAllocateN, #plan)
    local postActions = {}
    for i, v in ipairs(plan) do v(slots[i], postActions) end
    if not runProxyGroup(conn, postActions) then print(host .. ": postActions failed", 0xff0000) end
    busFreeN(slots)
    return "done"
  end)
end

local cycleBlue = proxyCycleTemplate("blue", function(plan, preActions)
  -- plantSower
  table.insert(preActions, {p = {op = "list", inv = "72e", side = sides.up}, f = function(p)
    local recipes = {
      {o = "Rice Seeds",     i = "Rice Seeds",     size = 512},
      {o = "Rice",           i = "Rice Seeds",     size = 512},
      {o = "Birch Sapling",  i = "Birch Sapling",  size = 512},
      {o = "Birch Wood",     i = "Birch Sapling",  size = 512},
      {o = "Soybean",        i = "Soybean",        size = 512, allowBackup = true},
      {o = "Strawberry",     i = "Strawberry",     size = 512, allowBackup = true},
      {o = "Spice Leaf",     i = "Spice Leaf",     size = 512},
      {o = "Beetroot Seeds", i = "Beetroot Seeds", size = 512},
      {o = "Beetroot",       i = "Beetroot Seeds", size = 512},
      {o = "Cotton",         i = "Cotton",         size = 512}
    }
    rankRecipes(recipes)
    local recipe = recipes[1]
    if recipe.demand > 0 then
      local inproc, toSlot = 0, 1
      for _, item in ipairs(p) do
        inproc = inproc + item.size
        if item.label == recipe.i then
          toSlot = item.slot
        elseif item.slot == toSlot then
          toSlot = toSlot + 1
        end
      end
      if toSlot <= 3 then
        local extraction = extractItem(recipe.i, 8 - inproc, "plantSower", recipe.allowBackup)
        if extraction.size > 0 then
          table.insert(plan, function(slot, postActions)
            extraction.extract(slot)
            table.insert(postActions, {p = {op = "xfer", inv = "72e", args = {sides.west, sides.up, extraction.size, slot, toSlot}}})
          end)
        end
      end
    end
  end})

  -- furnace
  table.insert(preActions, {p = {op = "list", inv = "eb7", side = sides.up}, f = function(p)
    local inproc, in1, in2 = 0
    for _, item in ipairs(p) do
      inproc = inproc + item.size
      if item.slot == 1 then
        in1 = item.label
      elseif item.slot == 3 then
        in2 = item.label
      else
        table.insert(plan, function(slot, postActions)
          table.insert(postActions, {p = {op = "xfer", inv = "eb7", args = {sides.up, sides.south, item.size, item.slot, slot}}})
        end)
      end
    end
    local recipes = {
      {o = "Glass",    i = "Sand",        size = getAvail("Sand")},
      {o = "Stone",    i = "Cobblestone", size = 512},
      {o = "Charcoal", i = "Birch Wood",  size = 512}
    }
    rankRecipes(recipes)
    local recipe = recipes[1]
    if recipe.demand > 0 then
      local toSlot
      if not in1 or recipe.i == in1 then toSlot = 1
      elseif not in2 or recipe.i == in2 then toSlot = 3 end
      if toSlot then
        local extraction = extractItem(recipe.i, math.min(recipe.size - recipe.oa, 16) - inproc, "furnace")
        if extraction.size > 0 then
          table.insert(plan, function(slot, postActions)
            extraction.extract(slot)
            table.insert(postActions, {p = {op = "xfer", inv = "eb7", args = {sides.south, sides.up, extraction.size, slot, toSlot}}})
          end)
        end
      end
    end
  end})

  -- manufactory
  table.insert(preActions, {p = {op = "list", inv = "eb7", side = sides.down}, f = function(p)
    local inproc, input = 0
    for _, item in ipairs(p) do
      inproc = inproc + item.size
      if item.slot == 1 then
        input = item.label
      else
        table.insert(plan, function(slot, postActions)
          table.insert(postActions, {p = {op = "xfer", inv = "eb7", args = {sides.down, sides.south, item.size, item.slot, slot}}})
        end)
      end
    end
    local recipes = {
      {o = "Sand",  i = "Cobblestone",  size = 512}
    }
    rankRecipes(recipes)
    local recipe = recipes[1]
    if recipe.demand > 0 then
      if not input or recipe.i == input then
        local extraction = extractItem(recipe.i, math.min(recipe.size - recipe.oa, 16) - inproc, "manufactory")
        if extraction.size > 0 then
          table.insert(plan, function(slot, postActions)
            extraction.extract(slot)
            table.insert(postActions, {p = {op = "xfer", inv = "eb7", args = {sides.south, sides.down, extraction.size, slot, 1}}})
          end)
        end
      end
    end
  end})

  -- squeezer
  do
    local inproc, input = 0
    table.insert(preActions, {p = {op = "list", inv = "fe6", side = sides.down}, f = function(p)
      for _, item in ipairs(p) do
        input = item.label
        if item.label == "Gravel" then
          inproc = item.size * 2
        else
          inproc = item.size
        end
      end
    end})
    table.insert(preActions, {p = {op = "list", inv = "fe6", side = sides.south}, f = function(p)
      for _, item in ipairs(p) do
        inproc = inproc + item.size
        table.insert(plan, function(slot, postActions)
          table.insert(postActions, {p = {op = "xfer", inv = "fe6", args = {sides.south, sides.north, item.size, item.slot, slot}}})
        end)
      end
      local recipes = {
        {o = "Gravel", i = "Cobblestone", size = 512},
        {o = "Flint",  i = "Gravel",      size = getAvail("Gravel")}
      }
      rankRecipes(recipes)
      local recipe = recipes[1]
      if recipe.demand > 0 then
        if not input or recipe.i == input then
          local toproc = math.min(recipe.size - recipe.oa, 16) - inproc
          if recipe.i == "Gravel" then toproc = math.ceil(toproc / 2) end
          local extraction = extractItem(recipe.i, toproc, "squeezer")
          if extraction.size > 0 then
            table.insert(plan, function(slot, postActions)
              extraction.extract(slot)
              table.insert(postActions, {p = {op = "xfer", inv = "fe6", args = {sides.north, sides.down, extraction.size, slot, 1}}})
            end)
          end
        end
      end
    end})
  end

  -- cobbleGen
  do
    local needed = 4096 - getAvail("Cobblestone")
    if needed > 0 then
      table.insert(plan, function(slot, postActions)
        table.insert(postActions, {p = {op = "xfer", inv = "fe6", args = {sides.up, sides.north, needed, 1, slot}}})
      end)
    end
  end

  -- heater
  table.insert(preActions, {p = {op = "list", inv = "eb7", side = sides.north}, f = function(p)
    local toproc = 8
    for _, item in ipairs(p) do
      toproc = toproc - item.size
    end
    if toproc > 0 then
      local extraction = extractItem("Charcoal", toproc, "boiler")
      if extraction.size > 0 then
        table.insert(plan, function(slot, postActions)
          extraction.extract(slot)
          table.insert(postActions, {p = {op = "xfer", inv = "eb7", args = {sides.south, sides.north, extraction.size, slot, 1}}})
        end)
      end
    end
  end})
end)

local cycleGreen = proxyCycleTemplate("green", function(plan, preActions)
  -- plantGatherer
  table.insert(preActions, {p = {op = "list", inv = "db6", side = sides.east}, f = function(p)
    for _, item in ipairs(p) do
      if item.slot > 4 then -- skip addon slots
        table.insert(plan, function(slot, postActions)
          table.insert(postActions, {p = {op = "xfer", inv = "db6", args = {sides.east, sides.down, item.size, item.slot, slot}}})
        end)
      end
    end
  end})

  -- presser
  do
    local inprocOut, inprocIn, input = 0, 0
    table.insert(preActions, {p = {op = "list", inv = "e51", side = sides.west}, f = function(p)
      for _, item in ipairs(p) do
        input = item.label
        inprocIn = item.size
      end
    end})
    table.insert(preActions, {p = {op = "list", inv = "db6", side = sides.north}, f = function(p)
      for _, item in ipairs(p) do
        inprocOut = math.max(inprocOut, item.size)
        table.insert(plan, function(slot, postActions)
          table.insert(postActions, {p = {op = "xfer", inv = "db6", args = {sides.north, sides.down, item.size, item.slot, slot}}})
        end)
      end
      local recipes = {
        {o = "Grain Bait",       i = "Soybean",     size = 512},
        {o = "Silken Tofu",      i = "Soybean",     size = 512},
        {o = "Firm Tofu",        i = "Silken Tofu", size = 512},
        {o = "Soy Milk",         i = "Silken Tofu", size = 512},
        {o = "Grain Bait",       i = "Strawberry",  size = 512},
        {o = "Strawberry Juice", i = "Strawberry",  size = 512},
        {o = "Fruit Bait",       i = "Strawberry",  size = 512}
      }
      rankRecipes(recipes)
      local recipe = recipes[1]
      if recipe.demand > 0 then
        if not input or recipe.i == input then
          local extraction = extractItem(recipe.i, math.min(recipe.size - recipe.oa, 16) - inprocIn - inprocOut, "presser")
          if extraction.size > 0 then
            table.insert(plan, function(slot, postActions)
              extraction.extract(slot)
              table.insert(postActions, {p = {op = "xfer", inv = "e51", args = {sides.east, sides.west, extraction.size, slot, 1}}})
            end)
          end
        end
      end
    end})
  end

  -- combustion
  table.insert(preActions, {p = {op = "list", inv = "201", side = sides.down}, f = function(p)
    for _, item in ipairs(p) do
      table.insert(plan, function(slot, postActions)
        table.insert(postActions, {p = {op = "xfer", inv = "201", args = {sides.down, sides.west, item.size, item.slot, slot}}})
      end)
    end
    local recipes = {
      {o = "Gunpowder",      is = {{item = "Flint", size = 1}},     size = 512, mult = 3},
      {o = "Blaze Powder",   is = {{item = "Gunpowder", size = 1}}, size = 512, mult = 3},
      {o = "Redstone",       is = {{item = "Blaze Powder", size = 2}, {item = "Gunpowder", size = 2}},   size = 512, mult = 4},
      {o = "Glowstone Dust", is = {{item = "Redstone", size = 4}, {item = "Blaze Powder", size = 2}},    size = 512, mult = 5},
      {o = "Netherrack",     is = {{item = "Cobblestone", size = 8}, {item = "Blaze Powder", size = 3}}, size = 512, mult = 8}
    }
    rankRecipes(recipes)
    local recipe = recipes[1]
    if recipe.demand > 0 then
      local nSets = math.min(math.ceil((recipe.size - recipe.oa) / recipe.mult), recipe.ia, 1)
      for _, ingredient in ipairs(recipe.is) do
        local extraction = extractItem(ingredient.item, ingredient.size * nSets, "combustion")
        table.insert(plan, function(slot, postActions)
          extraction.extract(slot)
          table.insert(postActions, {p = {op = "xfer", inv = "201", args = {sides.west, sides.north, extraction.size, slot, 1}}})
        end)
      end
    end
  end})
end)

local cycleBlack = proxyCycleTemplate("black", function(plan, preActions)
  -- mobCrusher
  table.insert(preActions, {p = {op = "list", inv = "091", side = sides.down}, f = function(p)
    for _, item in ipairs(p) do
      if item.slot > 4 then -- skip addon slots
        table.insert(plan, function(slot, postActions)
          table.insert(postActions, {p = {op = "xfer", inv = "091", args = {sides.down, sides.up, item.size, item.slot, slot}}})
        end)
      end
    end
  end})
end)

-- Main

for i = 1, #gpus do
  gpus[i].proxy.setResolution(table.unpack(gpus[i].resolution))
  gpus[i].proxy.setDepth(gpus[i].proxy.maxDepth())
  gpus[i].proxy.setBackground(0x000000)
end

local cycle
cycle = function()
  local t0 = computer.uptime()
  cpsChain(
    cycleStart,
    cpsAll(cycleBlue, cycleGreen, cycleBlack),
    cycleEnd
  )(function()
    lastCycleTime = computer.uptime() - t0
    cycle()
  end)
end
dispatch.queue(cycle)
dispatch.run()
