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
local sideDrawer = sides.south
local sideChest = sides.up
local sideBus = sides.down
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

local busAllocations, busWaitQueue, insertItem = {}, {}
local busUpdate = function()
  local stacks = inv.getAllStacks(sideBus)
  local freeSlots, recheck = {}
  for i = 1, stacks.count() do
    local item = stacks()
    if not busAllocations[i] then
      if not item.name then item.size = 0 end
      if item.size > 0 then
        print(item.label .. "*" .. math.floor(item.size), 0xffa500)
        insertItem(i, item)
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
  for _, v in ipairs(itemList) do
    result = math.min(result, math.floor(getAvail(v.item, v.allowBackup) / v.size))
  end
  return result
end

local rankRecipes = function(recipes, adjust, preAdjust)
  for _, recipe in ipairs(recipes) do
    if recipe.is then recipe.ia = getListAvail(recipe.is)
    else recipe.ia = getAvail(recipe.i, recipe.allowBackup) end
    if recipe.os then
      recipe.oa = 0
      for _, o in ipairs(recipe.os) do
        recipe.oa = recipe.oa + o.size * getAvail(o.item, true)
      end
    else
      recipe.oa = getAvail(recipe.o, true)
    end
    if preAdjust then preAdjust(recipe) end
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

local registerItem = function(item, side, slot)
  local old = labelMap[item.label]
  local provider = {size = item.size, side = side, slot = slot}
  if old then
    old.size = old.size + item.size
    table.insert(old.providers, provider)
  else
    item.providers = {provider}
    labelMap[item.label] = item
  end
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
  if x.backup and x.backup > x.size then x.backup = x.size end
  return qty
end

local extractItem = function(x, qty, reason, allowBackup)
  local result = {extract = function(slot) end}
  x = getItem(x)
  local toproc = math.floor(reserveItem(x, qty, allowBackup))
  result.size = toproc
  if toproc > 0 then
    if reason then reason = reason .. ": "
    else reason = "" end
    print(reason .. x.label .. "*" .. toproc, 0x55abec)
    result.extract = function(slot)
      while toproc > 0 do
        local provider = x.providers[#x.providers]
        local now = math.min(toproc, provider.size)
        inv.transferItem(provider.side, sideBus, now, provider.slot, slot)
        toproc = toproc - now
        provider.size = provider.size - now
        if provider.size <= 0 then table.remove(x.providers) end
      end
    end
  end
  return result
end

local drawerAcceptMap = {
  ["Cobblestone"] = 1, ["Piston"] = 1, ["Iron Plate"] = 1, ["Steel Plate"] = 1, ["Restonia Crystal"] = 1,
  ["Sand"] = 1, ["Glass"] = 1, ["Gravel"] = 1, ["Stone"] = 1, ["Block of Redstone"] = 1, ["Basic Coil"] = 1,
  ["Apatite"] = 1, ["Coal"] = 1, ["Redstone"] = 1, ["Obsidian"] = 1, ["Advanced Coil"] = 1,
  ["Emerald"] = 1, ["Lapis Lazuli"] = 1, ["Diamond"] = 1, ["Certus Quartz Crystal"] = 1, ["Signalum Ingot"] = 1,
  ["Iron Ingot"] = 1, ["Copper Ingot"] = 1, ["Gold Ingot"] = 1, ["Nickel Ingot"] = 1, ["Item Interface"] = 1,
  ["Tin Ingot"] = 1, ["Silver Ingot"] = 1, ["Lead Ingot"] = 1, ["Refined Uranium"] = 1, ["Bronze Gear"] = 1,
  ["Aluminum Ingot"] = 1, ["Rich Slag"] = 1, ["Slag"] = 1, ["Amber"] = 1, ["Oak Chest"] = 1,
  ["Black Quartz"] = 1, ["Cyanite Ingot"] = 1, ["Rare Earths"] = 1, ["Resonating Redstone Crystal"] = 1,
  ["Preserved Curiosity"] = 1, ["Draconium Dust"] = 1, ["Resonating Ore"] = 1, ["Platinum Ingot"] = 1,
  ["Oak Wood"] = 1, ["Oak Sapling"] = 1, ["Charcoal"] = 1, ["Pulverized Charcoal"] = 1, ["Firm Tofu"] = 1,
  ["Fluxed Phyto-Gro"] = 1, ["Rich Phyto-Gro"] = 1, ["Birch Sapling"] = 1, ["Birch Wood"] = 1,
  ["Nickel Ore"] = 1, ["Niter"] = 1, ["Sandstone"] = 1, ["Snowball"] = 1, ["Graphite Bar"] = 1,
  ["Blizz Powder"] = 1, ["Invar Ingot"] = 1, ["Electrum Ingot"] = 1, ["Bronze Ingot"] = 1, ["Sugar"] = 1,
  ["Cryotheum Dust"] = 1, ["Hardened Glass"] = 1, ["Steel Ingot"] = 1, ["Soybean"] = 1, ["Soy Milk"] = 1,
  ["Soybean Seed"] = 1, ["Mushroom"] = 1, ["Silken Tofu"] = 1, ["Grain Bait"] = 1, ["Sugar Canes"] = 1,
  ["Flour"] = 1, ["Plastic"] = 1, ["Dry Rubber"] = 1, ["Gunpowder"] = 1, ["Sulfur"] = 1, ["TNT"] = 1,
  ["item.projectred.core.itemResource.ruby.name"] = 1, ["Flax Seeds"] = 1, ["String"] = 1, ["Nether Quartz"] = 1,
  ["Melon"] = 1, ["Melon Seeds"] = 1, ["Dandelion"] = 1, ["Yellow Dye"] = 1, ["Raw Tofeeg"] = 1,
  ["item.projectred.core.itemResource.peridot.name"] = 1, ["Tomato Seed"] = 1, ["Tomato"] = 1, ["Iron Nugget"] = 1,
  ["item.projectred.core.itemResource.sapphire.name"] = 1, ["Seaweed"] = 1, ["Salt"] = 1, ["Flint"] = 1,
  ["Veggie Bait"] = 1, ["Seaweed Seed"] = 1, ["Fresh Water"] = 1, ["Dough"] = 1, ["Bread"] = 1,
  ["Butter"] = 1, ["Toast"] = 1, ["Ketchup"] = 1, ["Peppercorn Sapling"] = 1, ["Peppercorn"] = 1,
  ["Pumpkin"] = 1, ["Pumpkin Seeds"] = 1, ["Soy Sauce"] = 1, ["Cooking Oil"] = 1, ["Raw Tofeak"] = 1,
  ["Black Pepper"] = 1, ["Mustard Seed"] = 1, ["Mustard Seeds"] = 1, ["Cheese"] = 1, ["Fertilizer"] = 1,
  ["Cinnamon"] = 1, ["Spice Leaf"] = 1, ["Spice Leaf Seed"] = 1, ["Ginger"] = 1, ["Ginger Seed"] = 1,
  ["Corned Beef"] = 1, ["Bellpepper"] = 1, ["Onion"] = 1, ["Bellpepper Seed"] = 1, ["Onion Seed"] = 1,
  ["Potato"] = 1, ["Blaze Powder"] = 1, ["Wheat"] = 1, ["Seeds"] = 1, ["Poisonous Potato"] = 1, ["Cake"] = 1,
  ["Corned Beef Hash"] = 1, ["Corned Beef Breakfast"] = 1, ["Pyrotheum Dust"] = 1, ["Phyto-Gro"] = 1,
  ["Corn Grain"] = 1, ["Corn"] = 1, ["Cornmeal"] = 1, ["Steel Rod"] = 1, ["Steel Scaffolding"] = 1,
  ["Conveyor Belt"] = 1, ["Redstone Engineering Block"] = 1, ["Oak Wood Planks"] = 1, ["Sawdust"] = 1,
  ["Gold Nugget"] = 1, ["Energy Laser Relay"] = 1, ["Iron Mechanical Component"] = 1, ["Silver Gear"] = 1,
  ["Light Engineering Block"] = 1, ["Heavy Engineering Block"] = 1, ["Steel Mechanical Component"] = 1,
  ["Electrum Gear"] = 1, ["Hardened Upgrade Kit"] = 1, ["Reinforced Upgrade Kit"] = 1, ["Dark Steel Ingot"] = 1,
  ["Cactus"] = 1, ["Signalum Upgrade Kit"] = 1, ["Augment: Auxiliary Reception Coil"] = 1, ["Netherrack"] = 1,
  ["Redstone Reception Coil"] = 1, ["Augment: Auxiliary Sieve"] = 1, ["Redstone Servo"] = 1, ["Glowstone Dust"] = 1,
  ["Cobalt Ingot"] = 1, ["Ardite Ingot"] = 1, ["Manyullyn Ingot"] = 1, ["Lumium Ingot"] = 1, ["Lumium Gear"] = 1,
  ["Soul Sand"] = 1, ["Slimeball"] = 1, ["Nether Wart"] = 1, ["Grass"] = 1, ["Flint and Steel"] = 1,
  ["Grains of Infinity"] = 1, ["Iron Bars"] = 1, ["Industrial Dye Blend"] = 1, ["Lapis Lazuli Dust"] = 1,
  ["Crushed Quartz"] = 1, ["Organic Green Dye"] = 1, ["Green Dye"] = 1, ["Organic Black Dye"] = 1,
  ["Industrial Machine Chassis"] = 1, ["Simple Machine Chassis"] = 1, ["Cocoa Beans"] = 1, ["Soul Machine Chassis"] = 1,
  ["Soul Attuned Dye Blend"] = 1, ["Soul Powder"] = 1, ["Soularium Ingot"] = 1, ["Organic Brown Dye"] = 1,
  ["Pulverized Coal"] = 1, ["Infinity Bimetal Gear"] = 1, ["Dark Bimetal Gear"] = 1, ["Energetic Alloy Ingot"] = 1,
  ["Dark Steel Nugget"] = 1, ["Basic Capacitor"] = 1, ["Double-Layer Capacitor"] = 1, ["Iron Sheetmetal"] = 1,
  ["Iron Sheetmetal Slab"] = 1, ["Ender Pearl"] = 1, ["item.mob_ingredient_11.name"] = 1, ["Solidified Experience"] = 1,
  ["Enderman Head"] = 1, ["Enderium Base"] = 1, ["Enderium Ingot"] = 1, ["Resonant Upgrade Kit"] = 1,
  ["Vibrant Alloy Ingot"] = 1, ["Octadic Capacitor"] = 1, ["Glowstone"] = 1, ["Pulsating Iron Ingot"] = 1,
  ["Tin Gear"] = 1, ["Copper Gear"] = 1, ["Machine Frame"] = 1, ["Enderman Trophy"] = 1
}

insertItem = function(slot, item)
  if drawerAcceptMap[item.label] then
    inv.transferItem(sideBus, sideDrawer, item.size, slot)
  else
    inv.transferItem(sideBus, sideChest, item.size, slot)
  end
end

-- Handlers

cpsHandleInit = function(conn)
  while true do
    local p = conn.recv()
    if not p then return "done" end
    -- dispatch based on p.op here
  end
end

-- Cycle

local cycleCounter, lastCycleTime = 1, math.huge

local cycleStart = function(cont)
  print("Cycle #" .. cycleCounter .. ", lastCycleTime=" .. round(lastCycleTime * 100) / 100)
  labelMap = {}
  -- drawer
  forEachSlot(inv, sideDrawer, function(item, slot) registerItem(item, sideDrawer, slot) end)
  -- chest
  forEachSlot(inv, sideChest, function(item, slot) registerItem(item, sideChest, slot) end)
  -- backup seed items here
  backupItem("Oak Wood", 8)
  backupItem("Charcoal", 8)
  backupItem("Pulverized Charcoal", 8)
  backupItem("Rich Phyto-Gro", 8)
  backupItem("Fluxed Phyto-Gro", 8)
  backupItem("Mushroom", 8)
  backupItem("Sugar Canes", 8)
  backupItem("Gunpowder", 8)
  backupItem("Sulfur", 8)
  backupItem("Dandelion", 8)
  backupItem("Potato", 8)
  backupItem("Cactus", 8)
  backupItem("Netherrack", 8)
  backupItem("Redstone", 8)
  backupItem("Soul Sand", 8)
  backupItem("Grass", 8)
  backupItem("Pumpkin Seeds", 8)
  cont()
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

local slottedTemplate = function(plan, preActions, name, inv, sideCrafter, sideBus, inSlots, outFilter, recipes)
  table.insert(preActions, {p = {op = "list", inv = inv, side = sideCrafter}, f = function(p)
    local slotInfos = {}
    for _, slot in ipairs(inSlots) do slotInfos[slot] = {inproc = 0} end
    for _, item in ipairs(p) do
      local info = slotInfos[item.slot]
      if info then
        info.label = item.label
        info.inproc = item.size
      elseif not outFilter or outFilter(item) then
        table.insert(plan, function(slot, postActions)
          table.insert(postActions, {p = {op = "xfer", inv = inv, args = {sideCrafter, sideBus, item.size, item.slot, slot}}})
        end)
      end
    end
    rankRecipes(recipes)
    for _, recipe in ipairs(recipes) do
      if recipe.demand <= 0 then break end
      local sets, usedSlotsMap = recipe.ia, {}
      if not recipe.is then recipe.is = {{item = recipe.i, size = 1, slots = {inSlots[1]}, allowBackup = recipe.allowBackup}} end
      for index, ingredient in ipairs(recipe.is) do
        if not ingredient.slots then ingredient.slots = {inSlots[index]} end
        ingredient.eachSize = ingredient.size / #ingredient.slots
        for _, slot in ipairs(ingredient.slots) do
          local info = slotInfos[slot]
          if info.inproc > 0 and info.label ~= ingredient.item then
            sets = 0
            break
          else
            usedSlotsMap[slot] = true
            sets = math.min(sets, math.floor((recipe.maxInproc - info.inproc) / ingredient.eachSize))
          end
        end
        if sets <= 0 then break end
      end
      if sets > 0 then
        for slot, info in pairs(slotInfos) do
          if info.inproc > 0 and not usedSlotsMap[slot] then
            sets = 0
            break
          end
        end
      end
      if sets > 0 then
        for _, ingredient in ipairs(recipe.is) do
          local extraction = extractItem(ingredient.item, sets * ingredient.size, name, ingredient.allowBackup)
          table.insert(plan, function(slot, postActions)
            extraction.extract(slot)
            for _, toSlot in ipairs(ingredient.slots) do
              table.insert(postActions, {p = {op = "xfer", inv = inv, args = {sideBus, sideCrafter, ingredient.eachSize * sets, slot, toSlot}}})
            end
          end)
        end
        break
      end
    end
  end})
end

local workingSetTemplate = function(plan, preActions, inv, sideWorkingSet, sideBus, recipes)
  table.insert(preActions, {p = {op = "list", inv = inv, side = sideWorkingSet}, f = function(p)
    rankRecipes(recipes)
    local inprocMap = {}
    for _, recipe in ipairs(recipes) do inprocMap[recipe.i] = 0 end
    for _, item in ipairs(p) do
      if inprocMap[item.label] then
        inprocMap[item.label] = inprocMap[item.label] + item.size
      else
        table.insert(plan, function(slot, postActions)
          table.insert(postActions, {p = {op = "xfer", inv = inv, args = {sideWorkingSet, sideBus, item.size, item.slot, slot}}})
        end)
      end
    end
    for _, recipe in ipairs(recipes) do
      if recipe.demand <= 0 then break end
      local extraction = extractItem(recipe.i, recipe.maxInproc - inprocMap[recipe.i], recipe.name, recipe.allowBackup)
      if extraction.size > 0 then
        table.insert(plan, function(slot, postActions)
          extraction.extract(slot)
          table.insert(postActions, {p = {op = "xfer", inv = inv, args = {sideBus, sideWorkingSet, extraction.size, slot}}})
        end)
      end
    end
  end})
end

local heterogeneousWorkingSetTemplate = function(plan, preActions, name, inv, sideWorkingSet, sideBus, stockList, recipeMaxInproc, outFilter, recipes)
  table.insert(preActions, {p = {op = "list", inv = inv, side = sideWorkingSet}, f = function(p)
    for _, item in ipairs(p) do
      if stockList[item.label] then
        stockList[item.label] = stockList[item.label] - item.size
      elseif outFilter(item) then
        table.insert(plan, function(slot, postActions)
          table.insert(postActions, {p = {op = "xfer", inv = inv, args = {sideWorkingSet, sideBus, item.size, item.slot, slot}}})
        end)
      else
        recipeMaxInproc = recipeMaxInproc - item.size
      end
    end
    for item, maxInproc in pairs(stockList) do
      local extraction = extractItem(item, maxInproc, name)
      if extraction.size > 0 then
        table.insert(plan, function(slot, postActions)
          extraction.extract(slot)
          table.insert(postActions, {p = {op = "xfer", inv = inv, args = {sideBus, sideWorkingSet, extraction.size, slot}}})
        end)
      end
    end
    if recipeMaxInproc <= 0 then return end
    rankRecipes(recipes)
    for _, recipe in ipairs(recipes) do
      if recipe.demand <= 0 then break end
      if not recipe.is then recipe.is = {{item = recipe.i, size = 1, allowBackup = recipe.allowBackup}} end
      recipe.ia = getListAvail(recipe.is)
      local listSum = 0
      for _, ingredient in ipairs(recipe.is) do
        listSum = listSum + ingredient.size
      end
      local sets = math.min(recipe.ia, math.floor(recipeMaxInproc / listSum))
      if sets > 0 then
        for _, ingredient in ipairs(recipe.is) do
          local extraction = extractItem(ingredient.item, sets * ingredient.size, name, ingredient.allowBackup)
          table.insert(plan, function(slot, postActions)
            extraction.extract(slot)
            table.insert(postActions, {p = {op = "xfer", inv = inv, args = {sideBus, sideWorkingSet, extraction.size, slot}}})
          end)
        end
        recipeMaxInproc = recipeMaxInproc - sets * listSum
        if recipeMaxInproc <= 0 then break end
      end
    end
  end})
end

local inputlessTemplate = function(plan, preActions, inv, sideSource, sideBus, sourceSlot, needed)
  if needed <= 0 then return end
  table.insert(preActions, {p = {op = "list", inv = inv, side = sideSource}, f = function(p)
    local toproc = 0
    for _, item in ipairs(p) do if item.slot == sourceSlot then toproc = toproc + item.size end end
    if toproc > needed then toproc = needed end
    if toproc > 0 then
      table.insert(plan, function(slot, postActions)
        table.insert(postActions, {p = {op = "xfer", inv = inv, args = {sideSource, sideBus, needed, sourceSlot, slot}}})
      end)
    end
  end})
end

local analogCrafterTemplate = function(plan, preActions, name, inv, sideCrafter, sideBus, endSlot, recipes)
  local inSlots = {} for i = 1, endSlot do inSlots[i] = i end
  slottedTemplate(plan, preActions, name, inv, sideCrafter, sideBus, inSlots, function(x) return x.slot == 10 end, recipes)
end

local cycleBedrock = proxyCycleTemplate("bedrock", function(plan, preActions)
  -- infinity item
  workingSetTemplate(plan, preActions, "437", sides.down, sides.east, {
    {name = "infinity", size = 1, maxInproc = 1, i = "Flint and Steel"}
  })

  -- infinity redstone
  do
    local power = 0
    if getAvail("Grains of Infinity") < 1024 then power = 255 end
    table.insert(preActions, {p = {op = "call", inv = "242", fn = "setOutput", args = {sides.down, power}}})
  end
end)

local cycleElectrum = proxyCycleTemplate("electrum", function(plan, preActions)
  -- kekimurus
  workingSetTemplate(plan, preActions, "0f9", sides.down, sides.up, {
    {name = "kekimurus", size = 1, maxInproc = 1, i = "Cake"}
  })

  -- manaInfusion
  workingSetTemplate(plan, preActions, "2fd", sides.down, sides.up, {})

  -- conjuration
  heterogeneousWorkingSetTemplate(plan, preActions, "conjuration", "2fd", sides.east, sides.up, {}, 9, function() end, {
    {o = "Netherrack", i = "Netherrack", size = 1024, allowBackup = true},
    {o = "Redstone", i = "Redstone", size = 1024, allowBackup = true},
    {o = "Soul Sand", i = "Soul Sand", size = 1024, allowBackup = true},
    {o = "Grass", i = "Grass", size = 1024, allowBackup = true}
  })

  -- alchemy
  heterogeneousWorkingSetTemplate(plan, preActions, "alchemy", "2fd", sides.west, sides.up, {}, 9, function() end, {
    {o = "Glowstone Dust", i = "Redstone", size = 1024},
    {o = "Slimeball", i = "Cactus", size = 1024},
    {o = "Cocoa Beans", i = "Pumpkin Seeds", size = 128}
  })

  -- enderman redstone
  do
    local power = 0
    if getAvail("Ender Pearl") < 1024
      or getAvail("item.mob_ingredient_11.name") < 1024
      or getAvail("Enderman Head") < 1024
      or getAvail("Solidified Experience") < 1024
    then power = 15 end
    table.insert(preActions, {p = {op = "call", inv = "0bd", fn = "setOutput", args = {sides.up, power}}})
  end
end)

local cycleYellow = proxyCycleTemplate("yellow", function(plan, preActions)
  -- gearPress
  heterogeneousWorkingSetTemplate(plan, preActions, "gearPress", "4a0", sides.north, sides.up, {}, 16, function(x)
    return string.sub(x.label, #x.label - 4 + 1) == "Gear"
  end, {
    {o = "Silver Gear",   is = {{item = "Silver Ingot",   size = 4}}, size = 128},
    {o = "Bronze Gear",   is = {{item = "Bronze Ingot",   size = 4}}, size = 128},
    {o = "Electrum Gear", is = {{item = "Electrum Ingot", size = 4}}, size = 128},
    {o = "Lumium Gear",   is = {{item = "Lumium Ingot",   size = 4}}, size = 128},
    {o = "Tin Gear",      is = {{item = "Tin Ingot",      size = 4}}, size = 128},
    {o = "Copper Gear",   is = {{item = "Copper Ingot",   size = 4}}, size = 128}
  })

  -- alloySmelter
  slottedTemplate(plan, preActions, "alloySmelter", "4a0", sides.east, sides.up, {1, 2, 3}, nil, {
    {o = "Signalum Ingot", is = {{item = "Copper Ingot", size = 3}, {item = "Silver Ingot", size = 1}, {item = "Redstone", size = 10}}, size = 128, maxInproc = 40},
    {o = "Bronze Ingot", is = {{item = "Copper Ingot", size = 3}, {item = "Tin Ingot", size = 1}}, size = 128, maxInproc = 16},
    {o = "Enderium Base", is = {{item = "Lead Ingot", size = 3}, {item = "Platinum Ingot", size = 1}, {item = "Ender Pearl", size = 4}}, size = 128, maxInproc = 16},
    {o = "Invar Ingot", is = {{item = "Iron Ingot", size = 2}, {item = "Nickel Ingot", size = 1}}, size = 128, maxInproc = 16},
    {o = "Manyullyn Ingot", is = {{item = "Cobalt Ingot", size = 1}, {item = "Ardite Ingot", size = 1}}, size = 128, maxInproc = 16},
    {o = "Electrum Ingot", is = {{item = "Gold Ingot", size = 1}, {item = "Silver Ingot", size = 1}}, size = 128, maxInproc = 16},
    {o = "Lumium Ingot", is = {{item = "Tin Ingot", size = 3}, {item = "Silver Ingot", size = 1}, {item = "Glowstone Dust", size = 4}}, size = 128, maxInproc = 16},
    {o = "Soularium Ingot", is = {{item = "Soul Sand", size = 1}, {item = "Gold Ingot", size = 1}}, size = 128, maxInproc = 16},
    {o = "Energetic Alloy Ingot", is = {{item = "Gold Ingot", size = 1}, {item = "Redstone", size = 1}, {item = "Glowstone Dust", size = 1}}, size = 128, maxInproc = 16},
    {o = "Organic Green Dye", is = {{item = "Green Dye", size = 2}, {item = "Slimeball", size = 1}, {item = "Pulverized Charcoal", size = 2}}, size = 128, maxInproc = 16},
    {o = "Organic Brown Dye", is = {{item = "Cocoa Beans", size = 2}, {item = "Slimeball", size = 1}, {item = "Pulverized Charcoal", size = 2}}, size = 128, maxInproc = 16},
    {o = "Organic Black Dye", is = {{item = "Pulverized Charcoal", size = 6}, {item = "Slimeball", size = 1}}, size = 128, maxInproc = 10},
    {o = "Industrial Machine Chassis", is = {{item = "Simple Machine Chassis", size = 1}, {item = "Industrial Dye Blend", size = 1}}, size = 128, maxInproc = 16},
    {o = "Soul Machine Chassis", is = {{item = "Simple Machine Chassis", size = 1}, {item = "Soul Attuned Dye Blend", size = 1}}, size = 128, maxInproc = 16},
    {o = "Steel Ingot", is = {{item = "Iron Ingot", size = 1}, {item = "Pulverized Coal", size = 2}}, size = 128, maxInproc = 16},
    {o = "Enderium Ingot", is = {{item = "Enderium Base", size = 2}, {item = "Sand", size = 1}}, size = 128, maxInproc = 16},
    {o = "Vibrant Alloy Ingot", is = {{item = "Energetic Alloy Ingot", size = 1}, {item = "Ender Pearl", size = 1}}, size = 128, maxInproc = 16},
    {o = "Pulsating Iron Ingot", is = {{item = "Iron Ingot", size = 1}, {item = "Ender Pearl", size = 1}}, size = 128, maxInproc = 16}
  })

  -- xpTransposer
  slottedTemplate(plan, preActions, "xpTransposer", "f05", sides.up, sides.east, {1}, nil, {
    {o = "Blizz Powder", is = {{item = "Snowball", size = 2}}, size = 128, maxInproc = 16},
    {o = "Blaze Powder", is = {{item = "Sulfur",   size = 2}}, size = 128, maxInproc = 16}
  })

  -- IMC
  heterogeneousWorkingSetTemplate(plan, preActions, "IMC", "4a0", sides.south, sides.up, {}, 16, function() end, {
    {o = "Iron Mechanical Component", is = {{item = "Iron Plate", size = 2}, {item = "Copper Ingot", size = 1}}, size = 128}
  })

  -- SMC
  heterogeneousWorkingSetTemplate(plan, preActions, "SMC", "ba6", sides.south, sides.up, {}, 16, function() end, {
    {o = "Steel Mechanical Component", is = {{item = "Steel Plate", size = 2}, {item = "Copper Ingot", size = 1}}, size = 128}
  })

  -- factorizer
  slottedTemplate(plan, preActions, "factorizer", "4a0", sides.west, sides.up, {1}, nil, {
    {o = "Gold Nugget", i = "Gold Ingot", size = 128, maxInproc = 8},
    {o = "Iron Nugget", i = "Iron Ingot", size = 128, maxInproc = 8},
    {o = "Dark Steel Nugget", i = "Dark Steel Ingot", size = 128, maxInproc = 8}
  })

  -- obsidianInduction
  heterogeneousWorkingSetTemplate(plan, preActions, "obsidianInduction", "f05", sides.down, sides.east, {}, 16, function(x)
    return ({["Hardened Glass"] = 1, ["Dark Steel Ingot"] = 1})[x.label]
  end, {
    {o = "Hardened Glass", is = {{item = "Obsidian", size = 1}, {item = "Lead Ingot", size = 1}}, size = 128},
    {o = "Dark Steel Ingot", is = {{item = "Obsidian", size = 1}, {item = "Steel Ingot", size = 1}}, size = 128}
  })

  -- centrifuge
  slottedTemplate(plan, preActions, "centrifuge", "e67", sides.up, sides.west, {1}, nil, {
    {o = "Silken Tofu", i = "Soybean",     size = 128, maxInproc = 16},
    {o = "Grain Bait",  i = "Soybean",     size = 128, maxInproc = 16},
    {o = "Cooking Oil", i = "Pumpkin",     size = 128, maxInproc = 16},
    {o = "Soy Milk",    i = "Silken Tofu", size = 128, maxInproc = 16},
    {o = "Firm Tofu",   i = "Silken Tofu", size = 128, maxInproc = 16},
    {o = "Sugar",       i = "Sugar Canes", size = 128, maxInproc = 16}
  })

  -- yellowXNet
  workingSetTemplate(plan, preActions, "851", sides.west, sides.up, {
    {name = "plastic",    i = "Oak Wood",    o = "Dry Rubber",       size = 128,  maxInproc = 4},
    {name = "cinnamon",   i = "Phyto-Gro",   o = "Cinnamon",         size = 128,  maxInproc = 4},
    {name = "plateSteel", i = "Steel Ingot", o = "Steel Plate",      size = 128,  maxInproc = 16},
    {name = "plateIron",  i = "Iron Ingot",  o = "Iron Plate",       size = 128,  maxInproc = 16},
    {name = "restonia",   i = "Redstone",    o = "Restonia Crystal", size = 128,  maxInproc = 16},
    {name = "netherWart", i = "Mushroom",    o = "Nether Wart",      size = 1024, maxInproc = 4}
  })

  -- extractor
  slottedTemplate(plan, preActions, "extractor", "e67", sides.down, sides.west, {7}, function(x) return x.slot == 2 end, {
    {o = "Sulfur", i = "Gunpowder", size = 128, allowBackup = true, maxInproc = 16}
  })

  -- cuttingBoard
  analogCrafterTemplate(plan, preActions, "cuttingBoard", "ba6", sides.west, sides.up, 8, {
    {o = "Raw Tofeeg", size = 128, maxInproc = 16, is = {
      {item = "Yellow Dye", size = 1},
      {item = "Firm Tofu", size = 1}}},
    {o = "Raw Tofeak", size = 128, maxInproc = 16, is = {
      {item = "Mushroom", size = 1},
      {item = "Soy Sauce", size = 1},
      {item = "Black Pepper", size = 1},
      {item = "Cooking Oil", size = 1},
      {item = "Firm Tofu", size = 1}}}
  })

  -- juicer
  analogCrafterTemplate(plan, preActions, "juicer", "851", sides.down, sides.up, 8, {
    {o = "Ketchup", size = 128, maxInproc = 16, is = {
      {item = "Tomato", size = 1}}},
    {o = "Soy Sauce", size = 128, maxInproc = 16, is = {
      {item = "Fresh Water", size = 1},
      {item = "Soybean", size = 1},
      {item = "Salt", size = 1}}}
  })

  -- pot
  analogCrafterTemplate(plan, preActions, "pot", "e67", sides.south, sides.west, 8, {
    {o = "Salt", size = 128, maxInproc = 16, is = {
      {item = "Fresh Water", size = 1}}},
    {o = "Cheese", size = 128, maxInproc = 16, is = {
      {item = "Soy Milk", size = 1},
      {item = "Salt", size = 1}}},
    {o = "Corned Beef", size = 128, maxInproc = 16, is = {
      {item = "Raw Tofeak", size = 1},
      {item = "Sugar", size = 1},
      {item = "Cinnamon", size = 1},
      {item = "Mustard Seeds", size = 1},
      {item = "Peppercorn", size = 1},
      {item = "Spice Leaf", size = 1},
      {item = "Ginger", size = 1},
      {item = "Salt", size = 1}}}
  })

  -- skillet
  analogCrafterTemplate(plan, preActions, "skillet", "f05", sides.west, sides.east, 8, {
    {o = "Corned Beef Hash", size = 128, maxInproc = 16, is = {
      {item = "Cheese", size = 1},
      {item = "Butter", size = 1},
      {item = "Raw Tofeeg", size = 1},
      {item = "Potato", size = 1},
      {item = "Bellpepper", size = 1},
      {item = "Onion", size = 1},
      {item = "Corned Beef", size = 1}}},
    {o = "Corned Beef Breakfast", size = 1024, maxInproc = 16, is = {
      {item = "Ketchup", size = 1},
      {item = "Soy Milk", size = 1},
      {item = "Raw Tofeeg", size = 1},
      {item = "Toast", size = 1},
      {item = "Corned Beef Hash", size = 1}}}
  })

  -- pamMortar
  analogCrafterTemplate(plan, preActions, "skillet", "f05", sides.south, sides.east, 8, {
    {o = "Cornmeal", size = 128, maxInproc = 16, is = {
      {item = "Corn", size = 1}}}
  })

  -- richPhytoGro
  slottedTemplate(plan, preActions, "richPhytoGro", "ba6", sides.east, sides.up, {1, 2, 3}, function(x) return x.slot == 19 end, {
    {o = "Rich Phyto-Gro", size = 128, maxInproc = 16, is = {
      {item = "Pulverized Charcoal", size = 1, allowBackup = true},
      {item = "Rich Slag", size = 1},
      {item = "Niter", size = 1}}}
  })
end)

local cycleMarble = proxyCycleTemplate("marble", function(plan, preActions)
  local shouldInduceDust = getAvail("Slag", true) < 1024
  local shouldInduceOre = getAvail("Rich Slag", true) < 1024
  local dustsForInduction = {"Pulverized Iron", "Pulverized Nickel", "Pulverized Copper", "Pulverized Gold"}
  local oresForInduction = {"Copper Ore", "Gold Ore", "Iron Ore"}

  -- furnace / pulverizer output + sawmill
  workingSetTemplate(plan, preActions, "fbe", sides.north, sides.south, {
    {name = "sawmill", i = "Oak Wood", o = "Oak Wood Planks", size = 1024, maxInproc = 12}
  })

  -- furnace
  do
    local recipes = {
      {o = "Glass",        i = "Sand",        size = 1024},
      {o = "Stone",        i = "Cobblestone", size = 1024},
      {o = "Charcoal",     i = "Oak Wood",    size = 1024, allowBackup = true},
      {o = "Graphite Bar", i = "Charcoal",    size = 1024},
      {o = "Plastic",      i = "Dry Rubber",  size = 128},
      {o = "Green Dye",    i = "Cactus",      size = 128}
    }
    if not shouldInduceDust then
      for _, dust in ipairs(dustsForInduction) do
        table.insert(recipes, {i = dust, size = 1})
      end
    end
    heterogeneousWorkingSetTemplate(plan, preActions, "furnace", "fbe", sides.up, sides.south, {}, 27, function() end, recipes)
  end

  -- pulverizer
  do
    local recipes = {
      {o = "Sand",         i = "Cobblestone", size = 1024},
      {o = "Gravel",       i = "Stone",       size = 1024},
      {o = "Niter",        i = "Sandstone",   size = 1024},
      {o = "Flint",        i = "Gravel",      size = 128},
      {o = "Flour",        i = "Soybean",     size = 128},
      {o = "Yellow Dye",   i = "Dandelion",   size = 128},
      {o = "Veggie Bait",  i = "Seaweed",     size = 128},
      {o = "Salt",         i = "Seaweed",     size = 128},
      {o = "Black Pepper", i = "Peppercorn",  size = 128},
      {o = "Soul Powder",  i = "Soularium Ingot", size = 128},
      {o = "Crushed Quartz", i = "Nether Quartz", size = 128},
      {o = "Pulverized Coal", i = "Coal", size = 128},
      {o = "Lapis Lazuli Dust", i = "Lapis Lazuli", size = 128},
      {o = "Pulverized Charcoal", i = "Charcoal", size = 1024, allowBackup = true},
      {i = "Black Quartz Ore", size = 1}
    }
    if not shouldInduceOre then
      for _, ore in ipairs(oresForInduction) do
        table.insert(recipes, {i = ore, size = 1})
      end
    end
    rankRecipes(recipes)
    heterogeneousWorkingSetTemplate(plan, preActions, "pulverizer", "fbe", sides.down, sides.south, {}, 27, function() end, recipes)
  end

  -- sandInduction
  do
    local recipes = {
      {i = "Tin Ore",       size = 1},
      {i = "Aluminum Ore",  size = 1},
      {i = "Lead Ore",      size = 1},
      {i = "Silver Ore",    size = 1},
      {i = "Yellorite Ore", size = 1},
      {i = "Uranium Ore",   size = 1},
      {i = "Cobalt Ore",   size = 1},
      {i = "Ardite Ore",   size = 1}
    }
    if shouldInduceDust then
      for _, dust in ipairs(dustsForInduction) do
        table.insert(recipes, {i = dust, size = 1})
      end
    end
    if shouldInduceOre then
      for _, ore in ipairs(oresForInduction) do
        table.insert(recipes, {i = ore, size = 1})
      end
    end
    heterogeneousWorkingSetTemplate(plan, preActions, "sandInduction", "ebd", sides.up, sides.north, {["Sand"] = 13}, 13, function() end, recipes)
  end

  -- cobbleGen
  inputlessTemplate(plan, preActions, "ebd", sides.down, sides.north, 1, 8192 - getAvail("Cobblestone"))

  -- freshWater
  inputlessTemplate(plan, preActions, "742", sides.east, sides.south, 1, 128 - getAvail("Fresh Water"))

  -- obsGen
  inputlessTemplate(plan, preActions, "286", sides.up, sides.south, 1, 1024 - getAvail("Obsidian"))

  -- snowballGen
  inputlessTemplate(plan, preActions, "742", sides.up, sides.south, 1, 1024 - getAvail("Snowball"))

  -- atomicMiner output
  workingSetTemplate(plan, preActions, "26e", sides.north, sides.up, {})

  -- atomicMiner input
  do
    local stone = "Stone"
    if getAvail("Netherrack") > 0 and (getAvail("Nether Quartz") < 1024
      or getAvail("Ardite Ore") + getAvail("Ardite Ingot") < 128
      or getAvail("Cobalt Ore") + getAvail("Cobalt Ingot") < 128
    ) then stone = "Netherrack" end
    slottedTemplate(plan, preActions, "atomicMiner", "26e", sides.west, sides.up, {1}, nil, {{i = stone, size = 1, maxInproc = 16}})
  end

  -- induction
  slottedTemplate(plan, preActions, "induction", "286", sides.down, sides.south, {1, 2}, nil, {
    {is = {{item = "Cinnabar", size = 1}, {item = "Nickel Ore", size = 1}}, size = 1, maxInproc = 16},
    {is = {{item = "Rich Slag", size = 1}, {item = "Platinum Ore", size = 1}}, size = 1, maxInproc = 16}
  })

  -- reactor
  workingSetTemplate(plan, preActions, "afc", sides.north, sides.up, {{name = "reactor", i = "Refined Uranium", size = 1, maxInproc = 2}})

  -- analogCrafter
  analogCrafterTemplate(plan, preActions, "analogCrafter", "a48", sides.down, sides.north, 9, {
    {o = "Sandstone", is = {{item = "Sand", size = 4, slots = {1, 2, 4, 5}}}, size = 128, maxInproc = 16},
    {o = "Glowstone", is = {{item = "Glowstone Dust", size = 4, slots = {1, 2, 4, 5}}}, size = 128, maxInproc = 16},
    {o = "Block of Redstone", is = {{item = "Redstone", size = 9, slots = {1, 2, 3, 4, 5, 6, 7, 8, 9}}}, size = 128, maxInproc = 7},
    {o = "Oak Chest", is = {{item = "Oak Wood Planks", size = 8, slots = {1, 2, 3, 4, 6, 7, 8, 9}}}, size = 128, maxInproc = 8},
    {o = "Phyto-Gro", size = 128, maxInproc = 16, is = {
      {item = "Pulverized Charcoal", size = 1},
      {item = "Slag", size = 1},
      {item = "Niter", size = 1}}},
    {o = "Cryotheum Dust", size = 128, maxInproc = 16, is = {
      {item = "Blizz Powder", size = 2, slots = {1, 2}},
      {item = "Redstone", size = 1, slots = {3}},
      {item = "Snowball", size = 1, slots = {4}}}},
    {o = "Pyrotheum Dust", size = 128, maxInproc = 16, is = {
      {item = "Blaze Powder", size = 2, slots = {1, 2}},
      {item = "Redstone", size = 1, slots = {3}},
      {item = "Sulfur", size = 1, slots = {4}}}},
    {o = "Gunpowder", size = 128, maxInproc = 16, is = {
      {item = "Niter", size = 4, slots = {1, 2, 3, 4}},
      {item = "Sulfur", size = 1, slots = {5}, allowBackup = true},
      {item = "Charcoal", size = 1, slots = {6}}}},
    {o = "TNT", size = 128, maxInproc = 12, is = {
      {item = "Gunpowder", size = 5, slots = {1, 3, 5, 7, 9}},
      {item = "Sand", size = 4, slots = {2, 4, 6, 8}}}},
    {o = "Basic Coil", size = 128, maxInproc = 8, is = {
      {item = "Black Quartz", size = 1, slots = {5}},
      {item = "Restonia Crystal", size = 4, slots = {2, 4, 6, 8}}}},
    {o = "Advanced Coil", size = 128, maxInproc = 8, is = {
      {item = "Basic Coil", size = 1, slots = {5}},
      {item = "Gold Nugget", size = 8, slots = {1, 2, 3, 4, 6, 7, 8, 9}}}},
    {o = "Dark Bimetal Gear", size = 128, maxInproc = 8, is = {
      {item = "Infinity Bimetal Gear", size = 1, slots = {5}},
      {item = "Dark Steel Nugget", size = 8, slots = {1, 2, 3, 4, 6, 7, 8, 9}}}},
    {o = "Fertilizer", size = 128, maxInproc = 16, is = {
      {item = "Apatite", size = 1, slots = {5}},
      {item = "Sand", size = 2, slots = {2, 8}}}},
    {o = "Steel Rod", size = 128, maxInproc = 4, is = {
      {item = "Steel Ingot", size = 2, slots = {5, 8}}}},
    {o = "Steel Scaffolding", size = 128, maxInproc = 4, is = {
      {item = "Steel Rod", size = 3, slots = {1, 3, 5}},
      {item = "Steel Ingot", size = 3, slots = {7, 8, 9}}}},
    {o = "Conveyor Belt", size = 128, maxInproc = 4, is = {
      {item = "Redstone", size = 1, slots = {2}},
      {item = "Iron Ingot", size = 2, slots = {1, 3}},
      {item = "Plastic", size = 3, slots = {4, 5, 6}}}},
    {o = "Redstone Engineering Block", size = 128, maxInproc = 4, is = {
      {item = "Redstone", size = 4, slots = {2, 4, 6, 8}},
      {item = "Iron Ingot", size = 4, slots = {1, 3, 7, 9}},
      {item = "Copper Ingot", size = 1, slots = {5}}}},
    {o = "Light Engineering Block", size = 128, maxInproc = 4, is = {
      {item = "Iron Mechanical Component", size = 2, slots = {2, 8}},
      {item = "Iron Ingot", size = 4, slots = {1, 3, 7, 9}},
      {item = "Copper Ingot", size = 3, slots = {4, 5, 6}}}},
    {o = "Heavy Engineering Block", size = 128, maxInproc = 4, is = {
      {item = "Steel Mechanical Component", size = 2, slots = {2, 8}},
      {item = "Steel Ingot", size = 4, slots = {1, 3, 7, 9}},
      {item = "Electrum Ingot", size = 1, slots = {5}},
      {item = "Piston", size = 2, slots = {4, 6}}}},
    {o = "Piston", size = 128, maxInproc = 16, is = {
      {item = "Cobblestone", size = 4, slots = {1, 3, 4, 6}},
      {item = "Iron Ingot", size = 1, slots = {5}},
      {item = "Redstone", size = 1, slots = {2}},
      {item = "Oak Wood Planks", size = 3, slots = {7, 8, 9}}}},
    {o = "Energy Laser Relay", size = 128, maxInproc = 16, is = {
      {item = "Obsidian", size = 4, slots = {1, 3, 7, 9}},
      {item = "Block of Redstone", size = 2, slots = {2, 8}},
      {item = "Advanced Coil", size = 1, slots = {5}},
      {item = "Restonia Crystal", size = 2, slots = {4, 6}}}},
    {o = "Item Interface", size = 128, maxInproc = 8, is = {
      {item = "Redstone", size = 2, slots = {2, 8}},
      {item = "Oak Chest", size = 1, slots = {5}},
      {item = "Basic Coil", size = 4, slots = {1, 3, 7, 9}},
      {item = "Restonia Crystal", size = 2, slots = {4, 6}}}},
    {o = "Hardened Upgrade Kit", size = 128, maxInproc = 16, is = {
      {item = "Bronze Gear", size = 1, slots = {5}},
      {item = "Invar Ingot", size = 4, slots = {2, 4, 6, 8}},
      {item = "Redstone", size = 2, slots = {1, 3}}}},
    {o = "Reinforced Upgrade Kit", size = 128, maxInproc = 16, is = {
      {item = "Silver Gear", size = 1, slots = {5}},
      {item = "Electrum Ingot", size = 4, slots = {2, 4, 6, 8}},
      {item = "Hardened Glass", size = 2, slots = {1, 3}}}},
    {o = "Signalum Upgrade Kit", size = 128, maxInproc = 16, is = {
      {item = "Electrum Gear", size = 1, slots = {5}},
      {item = "Signalum Ingot", size = 4, slots = {2, 4, 6, 8}},
      {item = "Cryotheum Dust", size = 2, slots = {1, 3}}}},
    {o = "Resonant Upgrade Kit", size = 128, maxInproc = 16, is = {
      {item = "Lumium Gear", size = 1, slots = {5}},
      {item = "Enderium Ingot", size = 4, slots = {2, 4, 6, 8}},
      {item = "Pyrotheum Dust", size = 2, slots = {1, 3}}}},
    {o = "Redstone Reception Coil", size = 128, maxInproc = 16, is = {
      {item = "Redstone", size = 2, slots = {3, 7}},
      {item = "Gold Ingot", size = 1, slots = {5}}}},
    {o = "Augment: Auxiliary Reception Coil", size = 128, maxInproc = 16, is = {
      {item = "Redstone Reception Coil", size = 1, slots = {5}},
      {item = "Gold Ingot", size = 4, slots = {2, 4, 6, 8}}}},
    {o = "Augment: Auxiliary Sieve", size = 128, maxInproc = 16, is = {
      {item = "Redstone Servo", size = 1, slots = {5}},
      {item = "Bronze Ingot", size = 4, slots = {2, 4, 6, 8}}}},
    {o = "Redstone Servo", size = 128, maxInproc = 16, is = {
      {item = "Redstone", size = 2, slots = {2, 8}},
      {item = "Iron Ingot", size = 1, slots = {5}}}},
    {o = "Flint and Steel", size = 128, maxInproc = 2, is = {
      {item = "Flint", size = 1},
      {item = "Iron Ingot", size = 1}}},
    {o = "Iron Bars", size = 128, maxInproc = 10, is = {
      {item = "Iron Ingot", size = 6, slots = {1, 2, 3, 4, 5, 6}}}},
    {o = "Simple Machine Chassis", size = 128, maxInproc = 16, is = {
      {item = "Iron Bars", size = 4, slots = {1, 3, 7, 9}},
      {item = "Iron Ingot", size = 4, slots = {2, 4, 6, 8}},
      {item = "Grains of Infinity", size = 1, slots = {5}}}},
    {o = "Machine Frame", size = 128, maxInproc = 16, is = {
      {item = "Iron Ingot", size = 4, slots = {1, 3, 7, 9}},
      {item = "Glass", size = 4, slots = {2, 4, 6, 8}},
      {item = "Tin Gear", size = 1, slots = {5}}}},
    {o = "Industrial Dye Blend", size = 128, maxInproc = 16, is = {
      {item = "Lapis Lazuli Dust", size = 2, slots = {1, 9}},
      {item = "Crushed Quartz", size = 4, slots = {2, 4, 6, 8}},
      {item = "Organic Green Dye", size = 2, slots = {3, 7}},
      {item = "Organic Black Dye", size = 1, slots = {5}}}},
    {o = "Soul Attuned Dye Blend", size = 128, maxInproc = 16, is = {
      {item = "Soul Powder", size = 2, slots = {1, 9}},
      {item = "Crushed Quartz", size = 4, slots = {2, 4, 6, 8}},
      {item = "Organic Brown Dye", size = 2, slots = {3, 7}},
      {item = "Organic Black Dye", size = 1, slots = {5}}}},
    {o = "Infinity Bimetal Gear", size = 128, maxInproc = 16, is = {
      {item = "Iron Nugget", size = 4, slots = {1, 3, 7, 9}},
      {item = "Iron Ingot", size = 4, slots = {2, 4, 6, 8}},
      {item = "Grains of Infinity", size = 1, slots = {5}}}},
    {o = "Basic Capacitor", size = 128, maxInproc = 16, is = {
      {item = "Grains of Infinity", size = 2, slots = {3, 7}},
      {item = "Gold Nugget", size = 4, slots = {2, 4, 6, 8}},
      {item = "Copper Ingot", size = 1, slots = {5}}}},
    {o = "Double-Layer Capacitor", size = 128, maxInproc = 16, is = {
      {item = "Basic Capacitor", size = 2, slots = {4, 6}},
      {item = "Energetic Alloy Ingot", size = 2, slots = {2, 8}},
      {item = "Pulverized Coal", size = 1, slots = {5}}}},
    {o = "Octadic Capacitor", size = 128, maxInproc = 16, is = {
      {item = "Double-Layer Capacitor", size = 2, slots = {4, 6}},
      {item = "Vibrant Alloy Ingot", size = 2, slots = {2, 8}},
      {item = "Glowstone", size = 1, slots = {5}}}},
    {o = "Iron Sheetmetal", size = 128, maxInproc = 16, is = {
      {item = "Iron Plate", size = 4, slots = {2, 4, 6, 8}}}},
    {o = "Iron Sheetmetal Slab", size = 128, maxInproc = 16, is = {
      {item = "Iron Sheetmetal", size = 3, slots = {1, 2, 3}}}},
    {o = "Cake", size = 128, maxInproc = 2, is = {
      {item = "Soy Milk", size = 3, slots = {7, 8, 9}},
      {item = "Sugar", size = 2, slots = {4, 6}},
      {item = "Raw Tofeeg", size = 1, slots = {5}},
      {item = "Flour", size = 3, slots = {1, 2, 3}}}}
  })

  -- mixingBowl
  analogCrafterTemplate(plan, preActions, "mixingBowl", "a48", sides.east, sides.north, 8, {
    {o = "Dough", size = 128, maxInproc = 16, is = {
      {item = "Fresh Water", size = 1},
      {item = "Flour", size = 1},
      {item = "Salt", size = 1}}}
  })

  -- bakeware
  analogCrafterTemplate(plan, preActions, "bakeware", "286", sides.east, sides.south, 8, {
    {o = "Bread", size = 128, maxInproc = 16, is = {
      {item = "Dough", size = 1}}},
    {o = "Toast", size = 128, maxInproc = 16, is = {
      {item = "Bread", size = 1},
      {item = "Butter", size = 1}}}
  })

  -- saucepan
  analogCrafterTemplate(plan, preActions, "saucepan", "ebd", sides.east, sides.north, 8, {
    {o = "Butter", size = 128, maxInproc = 16, is = {
      {item = "Silken Tofu", size = 1},
      {item = "Salt", size = 1}}}
  })

  -- phyto
  slottedTemplate(plan, preActions, "phyto", "a48", sides.up, sides.north, {1, 2}, nil, {
    {o = "Oak Wood",           is = {{item = "Fluxed Phyto-Gro", size = 1, allowBackup = true}, {item = "Oak Sapling", size = 1}},   size = 1024, maxInproc = 16},
    {o = "Oak Sapling",        is = {{item = "Fluxed Phyto-Gro", size = 1, allowBackup = true}, {item = "Oak Sapling", size = 1}},   size = 1024, maxInproc = 16},
    {o = "Sugar Canes",        is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Sugar Canes", size = 1, allowBackup = true}},   size = 1024, maxInproc = 16},
    {o = "Mushroom",           is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Mushroom", size = 1, allowBackup = true}},      size = 1024, maxInproc = 16},
    {o = "Cactus",             is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Cactus", size = 1, allowBackup = true}},        size = 1024, maxInproc = 16},
    {o = "Potato",             is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Potato", size = 1, allowBackup = true}},        size = 128,  maxInproc = 16},
    {o = "Poisonous Potato",   is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Potato", size = 1, allowBackup = true}},        size = 128,  maxInproc = 16},
    {o = "Dandelion",          is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Dandelion", size = 1, allowBackup = true}},     size = 128,  maxInproc = 16},
    {o = "Birch Wood",         is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Birch Sapling", size = 1}},                     size = 1024, maxInproc = 16},
    {o = "Birch Sapling",      is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Birch Sapling", size = 1}},                     size = 128,  maxInproc = 16},
    {o = "Soybean",            is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Soybean Seed", size = 1}},                      size = 1024, maxInproc = 16},
    {o = "Soybean Seed",       is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Soybean Seed", size = 1}},                      size = 128,  maxInproc = 16},
    {o = "String",             is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Flax Seeds", size = 1}},                        size = 1024, maxInproc = 16},
    {o = "Flax Seeds",         is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Flax Seeds", size = 1}},                        size = 128,  maxInproc = 16},
    {o = "Seaweed",            is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Seaweed Seed", size = 1}},                      size = 128,  maxInproc = 16},
    {o = "Seaweed Seed",       is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Seaweed Seed", size = 1}},                      size = 128,  maxInproc = 16},
    {o = "Melon",              is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Melon Seeds", size = 1}},                       size = 128,  maxInproc = 16},
    {o = "Melon Seeds",        is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Melon Seeds", size = 1}},                       size = 128,  maxInproc = 16},
    {o = "Tomato",             is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Tomato Seed", size = 1}},                       size = 128,  maxInproc = 16},
    {o = "Tomato Seed",        is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Tomato Seed", size = 1}},                       size = 128,  maxInproc = 16},
    {o = "Peppercorn",         is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Peppercorn Sapling", size = 1}},                size = 128,  maxInproc = 16},
    {o = "Peppercorn Sapling", is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Peppercorn Sapling", size = 1}},                size = 128,  maxInproc = 16},
    {o = "Pumpkin",            is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Pumpkin Seeds", size = 1, allowBackup = true}}, size = 128,  maxInproc = 16},
    {o = "Pumpkin Seeds",      is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Pumpkin Seeds", size = 1, allowBackup = true}}, size = 128,  maxInproc = 16},
    {o = "Mustard Seeds",      is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Mustard Seed", size = 1}},                      size = 128,  maxInproc = 16},
    {o = "Mustard Seed",       is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Mustard Seed", size = 1}},                      size = 128,  maxInproc = 16},
    {o = "Spice Leaf",         is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Spice Leaf Seed", size = 1}},                   size = 128,  maxInproc = 16},
    {o = "Spice Leaf Seed",    is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Spice Leaf Seed", size = 1}},                   size = 128,  maxInproc = 16},
    {o = "Ginger",             is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Ginger Seed", size = 1}},                       size = 128,  maxInproc = 16},
    {o = "Ginger Seed",        is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Ginger Seed", size = 1}},                       size = 128,  maxInproc = 16},
    {o = "Bellpepper",         is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Bellpepper Seed", size = 1}},                   size = 128,  maxInproc = 16},
    {o = "Bellpepper Seed",    is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Bellpepper Seed", size = 1}},                   size = 128,  maxInproc = 16},
    {o = "Onion",              is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Onion Seed", size = 1}},                        size = 128,  maxInproc = 16},
    {o = "Onion Seed",         is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Onion Seed", size = 1}},                        size = 128,  maxInproc = 16},
    {o = "Wheat",              is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Seeds", size = 1}},                             size = 128,  maxInproc = 16},
    {o = "Seeds",              is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Seeds", size = 1}},                             size = 128,  maxInproc = 16},
    {o = "Corn",               is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Corn Grain", size = 1}},                        size = 128,  maxInproc = 16},
    {o = "Corn Grain",         is = {{item = "Fluxed Phyto-Gro", size = 1}, {item = "Corn Grain", size = 1}},                        size = 128,  maxInproc = 16}
  })

  -- charger
  slottedTemplate(plan, preActions, "charger", "742", sides.down, sides.south, {1}, nil, {
    {o = "Fluxed Phyto-Gro", i = "Rich Phyto-Gro", size = 1024, allowBackup = true, maxInproc = 16}
  })
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
    cpsAll(cycleMarble, cycleYellow, cycleElectrum, cycleBedrock),
    cycleEnd
  )(function()
    lastCycleTime = computer.uptime() - t0
    cycle()
  end)
end
dispatch.queue(cycle)
dispatch.run()
