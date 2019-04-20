-- recommended setup:
-- 1 chunk for automation; 1 neighboring chunk for non-automated stuff
-- each floor has 1 proxy
-- central floor has factory+reactor+proxy
-- color code each floor by floor block

local round = function(num)
  return math.floor(num + 0.5)
end

local startsWith = function(pattern, input)
  return string.sub(input, 1, #pattern) == pattern
end

local endsWith = function(pattern, input)
  return string.sub(input, #input - #pattern + 1) == pattern
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
    if #buf > unack and wnd > unack then
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
      if wnd > unack then
        transmit()
      end
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

local ambiguousLabelMap = {
  -- List ambiguous item labels here
  ["Clay"] = 1,
  ["Melon"] = 1,
  ["Mushroom"] = 1,
  ["Nether Brick"] = 1,
  ["Machine Frame"] = 1,
  ["Inferium Seeds"] = 1
}

local itemIdentifier = function(item)
  if ambiguousLabelMap[item.label] then
    return item.label .. ";" .. item.name
  else
    return item.label
  end
end

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

local busAllocations, busWaitQueue, insertItem, shouldDestroy = {}, {}
local busUpdate = function()
  local stacks = inv.getAllStacks(sideBus)
  local freeSlots, recheck = {}
  for i = 1, stacks.count() do
    local item = stacks()
    if not busAllocations[i] then
      if not item.name then item.size = 0 end
      if item.size > 0 then
        print(itemIdentifier(item).. "*" .. math.floor(item.size), 0xffa500)
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

local itemMap = {}

local getItem = function(x)
  if type(x) == "string" then return itemMap[x] else return x end
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
  local key = itemIdentifier(item)
  local old = itemMap[key]
  local provider = {size = item.size, side = side, slot = slot}
  if old then
    old.size = old.size + item.size
    table.insert(old.providers, provider)
  else
    item.providers = {provider}
    itemMap[key] = item
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
    print(reason .. itemIdentifier(x) .. "*" .. toproc, 0x55abec)
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
  ["Cobblestone"] = 1
  -- define drawer filter here
}

insertItem = function(slot, item)
  -- handle shouldDestroy here
  if drawerAcceptMap[itemIdentifier(item)] then
    inv.transferItem(sideBus, sideDrawer, item.size, slot)
  else
    inv.transferItem(sideBus, sideChest, item.size, slot)
  end
end

-- Handlers

local cycleHooks = {}

local handleStore = function(conn, p)
  local c = coroutine.yield("then", function(c) table.insert(cycleHooks, c) end)
  local slots = coroutine.yield("then", busAllocateN, p.n, p.allowPartial)
  conn.send{slots = slots}
  conn.recv()
  busFreeN(slots)
  c()
end

cpsHandleInit = function(conn)
  while true do
    local p = conn.recv()
    if not p then return "done" end
    if p.op == "store" then
      handleStore(conn, p)
    else
      conn.close()
    end
  end
end

-- Cycle

local cycleCounter, lastCycleTime = 1, math.huge

local cycleStart = function(cont)
  print("Cycle #" .. cycleCounter .. ", lastCycleTime=" .. round(lastCycleTime * 100) / 100)
  itemMap = {}
  -- drawer
  forEachSlot(inv, sideDrawer, function(item, slot) registerItem(item, sideDrawer, slot) end)
  -- chest
  forEachSlot(inv, sideChest, function(item, slot) registerItem(item, sideChest, slot) end)
  -- backup seed items here
  cont()
end

shouldDestroy = function(item)
  -- filter junks here
  return false
end

local cycleRunHooks = function(c)
  local hooks = cycleHooks
  cycleHooks = {}
  cpsAll(table.unpack(hooks))(c)
end

local cycleEnd = function(cont)
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

-- fn: (plan, preActions, [postActions]) -> ()
-- plan: {(slot, postActions) -> (), ...}
local proxyCycleTemplate = function(host, fn)
  local conn = wrappedConnUtils(host)
  return cpsWrap(function()
    local plan, preActions, postActions = {}, {}, {}
    fn(plan, preActions, postActions)
    if not runProxyGroup(conn, preActions) then print(host .. ": preActions failed", 0xff0000) end
    if #plan <= 0 then
      if not runProxyGroup(conn, postActions) then print(host .. ": postActions failed", 0xff0000) end
      return "done"
    end
    local slots = coroutine.yield("then", busAllocateN, #plan)
    for i, v in ipairs(plan) do v(slots[i], postActions) end
    if not runProxyGroup(conn, postActions) then print(host .. ": postActions failed", 0xff0000) end
    busFreeN(slots)
    return "done"
  end)
end

local slottedTemplate = function(plan, preActions, name, inv, sideCrafter, sideBus, inSlots, outFilter, recipes, fnSkip)
  if not outFilter then
    rankRecipes(recipes)
    if not recipes[1] or recipes[1].demand <= 0 then return end
  end
  table.insert(preActions, {p = {op = "list", inv = inv, side = sideCrafter}, f = function(p)
    local slotInfos = {}
    for _, slot in ipairs(inSlots) do slotInfos[slot] = {inproc = 0} end
    for _, item in ipairs(p) do
      local info = slotInfos[item.slot]
      if info then
        info.key = itemIdentifier(item)
        info.inproc = item.size
      elseif outFilter and outFilter(item) then
        table.insert(plan, function(slot, postActions)
          table.insert(postActions, {p = {op = "xfer", inv = inv, args = {sideCrafter, sideBus, item.size, item.slot, slot}}})
        end)
      end
    end
    if fnSkip and fnSkip() then return end
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
          if info.inproc > 0 and info.key ~= ingredient.item then
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

local workingSetTemplate = function(plan, preActions, inv, sideWorkingSet, sideBus, outFilter, recipes, fnSkip)
  if not outFilter then
    rankRecipes(recipes)
    if not recipes[1] or recipes[1].demand <= 0 then return end
  end
  table.insert(preActions, {p = {op = "list", inv = inv, side = sideWorkingSet}, f = function(p)
    rankRecipes(recipes)
    local inprocMap = {}
    for _, recipe in ipairs(recipes) do inprocMap[recipe.i] = 0 end
    for _, item in ipairs(p) do
      local key = itemIdentifier(item)
      if inprocMap[key] then
        inprocMap[key] = inprocMap[key] + item.size
      elseif outFilter and outFilter(item) then
        table.insert(plan, function(slot, postActions)
          table.insert(postActions, {p = {op = "xfer", inv = inv, args = {sideWorkingSet, sideBus, item.size, item.slot, slot}}})
        end)
      end
    end
    if fnSkip and fnSkip() then return end
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

local heterogeneousWorkingSetTemplate = function(plan, preActions, name, inv, sideWorkingSet, sideBus, stockList, recipeMaxInproc, outFilter, recipes, fnSkip)
  if not (stockList or outFilter) then
    rankRecipes(recipes)
    if not recipes[1] or recipes[1].demand <= 0 then return end
  end
  table.insert(preActions, {p = {op = "list", inv = inv, side = sideWorkingSet}, f = function(p)
    for _, item in ipairs(p) do
      local key = itemIdentifier(item)
      if stockList and stockList[key] then
        stockList[key] = stockList[key] - item.size
      elseif outFilter and outFilter(item) then
        table.insert(plan, function(slot, postActions)
          table.insert(postActions, {p = {op = "xfer", inv = inv, args = {sideWorkingSet, sideBus, item.size, item.slot, slot}}})
        end)
      else
        recipeMaxInproc = recipeMaxInproc - item.size
      end
    end
    if fnSkip and fnSkip() then return end
    if stockList then
      for item, maxInproc in pairs(stockList) do
        local extraction = extractItem(item, maxInproc, name)
        if extraction.size > 0 then
          table.insert(plan, function(slot, postActions)
            extraction.extract(slot)
            table.insert(postActions, {p = {op = "xfer", inv = inv, args = {sideBus, sideWorkingSet, extraction.size, slot}}})
          end)
        end
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
        table.insert(postActions, {p = {op = "xfer", inv = inv, args = {sideSource, sideBus, toproc, sourceSlot, slot}}})
      end)
    end
  end})
end

local heterogeneousInputlessTemplate = function(plan, preActions, inv, sideSource, sideBus, needed)
  table.insert(preActions, {p = {op = "list", inv = inv, side = sideSource}, f = function(p)
    local availMap = {}
    for _, item in ipairs(p) do
      local key = itemIdentifier(item)
      if not availMap[key] then availMap[key] = getAvail(key, true) end
      local toproc = math.min(needed - availMap[key], item.size)
      if toproc > 0 then
        availMap[key] = availMap[key] + toproc
        table.insert(plan, function(slot, postActions)
          table.insert(postActions, {p = {op = "xfer", inv = inv, args = {sideSource, sideBus, toproc, item.slot, slot}}})
        end)
      end
    end
  end})
end

local analogCrafterTemplate = function(plan, preActions, name, inv, sideCrafter, sideBus, endSlot, recipes)
  local inSlots = {} for i = 1, endSlot do inSlots[i] = i end
  slottedTemplate(plan, preActions, name, inv, sideCrafter, sideBus, inSlots, function(x) return x.slot == 10 end, recipes)
end

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
    cycleRunHooks,
    cycleEnd
  )(function()
    lastCycleTime = computer.uptime() - t0
    if lastCycleTime < 1 then
      dispatch.setAlarm(t0 + 1, cycle)
    else
      cycle()
    end
  end)
end
dispatch.queue(cycle)
dispatch.run()
