-- factory controller script
-- by cybcaoyibo

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
  local percQ = "%q"
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
      or percQ:format(s):gsub("\010","n"):gsub("\026","\\026") end
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

local modem = component.proxy(resolve("modem"))
local inv = component.proxy(resolve("transposer"))
local dbAddr = resolve("database")
local db = component.proxy(dbAddr)
local me = component.proxy(resolve("5ec"))
local channel = 1
local hostname = "factory"
local sideBus = sides.up
local sideME = sides.down
local sideDrawer = sides.west
local gpus = {
  {proxy = component.proxy(resolve("gpu")), resolution = {80, 25}}
}

local inferiumSeeds = "Inferium Seeds;mysticalagriculture:tier5_inferium_seeds"
local ambiguousLabelMap = {
  -- List ambiguous item labels here
  ["Clay"] = 1,
  ["Melon"] = 1,
  ["Amber"] = 1,
  ["Flour"] = 1,
  ["Mushroom"] = 1,
  ["Connector"] = 1,
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

local busAllocations, busWaitQueue, insertItem = {}, {}
local busUpdate = function()
  local stacks = inv.getAllStacks(sideBus)
  local freeSlots, recheck = {}
  for i = 1, stacks.count() do
    local item = stacks()
    if not busAllocations[i] then
      if not item.name then item.size = 0 end
      if item.size > 0 then
        print(itemIdentifier(item) .. "*" .. math.floor(item.size) .. insertItem(i, item), 0xffa500)
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
      if x.drawerInfo then
        local now = math.min(x.drawerInfo.size, toproc)
        inv.transferItem(sideDrawer, sideBus, now, x.drawerInfo.slot, slot)
        toproc = toproc - now
        x.drawerInfo.size = x.drawerInfo.size - now
        if x.drawerInfo.size <= 0 then x.drawerInfo = nil end
      end
      if toproc > 0 then
        db.clear(1)
        me.store({label = x.label, name = x.name}, dbAddr, 1, 1)
        me.setInterfaceConfiguration(1, dbAddr, 1, toproc)
        inv.transferItem(sideME, sideBus, toproc, 1, slot)
        me.setInterfaceConfiguration(1)
      end
    end
  end
  return result
end

insertItem = function(slot, item)
  local stored = itemMap[itemIdentifier(item)]
  if stored and stored.drawerInfo then
    inv.transferItem(sideBus, sideDrawer, item.size, slot)
    return " -> Drawer"
  else
    inv.transferItem(sideBus, sideME, item.size, slot, 9)
    return " -> AE2"
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
  itemMap = {}
  -- ae2
  for _, item in ipairs(me.getItemsInNetwork()) do
    itemMap[itemIdentifier(item)] = item
  end
  -- drawer
  forEachSlot(inv, sideDrawer, function(item, slot)
    local info = itemMap[itemIdentifier(item)]
    if info then info.drawerInfo = {size = item.size, slot = slot} end
  end)
  -- backup seed items here
  backupItem("Seeds", 8)
  backupItem("Birch Sapling", 8)
  backupItem("Carrot", 8)
  backupItem("Cotton", 8)
  backupItem("Soybean", 8)
  backupItem("Beetroot Seeds", 8)
  backupItem("Tomato", 8)
  backupItem("Coffee Beans", 8)
  backupItem("Netherrack", 8)
  backupItem("Potato", 8)
  backupItem("Peppercorn Sapling", 8)
  backupItem("Tea Leaf", 8)
  backupItem("Mustard Seeds", 8)
  backupItem("Spice Leaf", 8)
  backupItem("Ginger", 8)
  backupItem("Onion", 8)
  backupItem("Bellpepper", 8)
  backupItem(inferiumSeeds, 8)
  backupItem("Nature Seeds", 8)
  backupItem("Sulfur Seeds", 8)
  backupItem("Nether Quartz Seeds", 8)
  backupItem("Experience Seeds", 8)
  backupItem("Enderman Seeds", 8)
  backupItem("Mango Sapling", 8)
  backupItem("Coconut Sapling", 8)
  backupItem("Garlic", 8)
  backupItem("Rice", 8)
  backupItem("Chili Pepper", 8)
  backupItem("Fiery Ingot Seeds", 8)
  backupItem("Emerald Seeds", 8)
  backupItem("Redstone Seeds", 8)
  cont()
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

local cycleCenter = proxyCycleTemplate("center", function(plan, preActions)
  -- gas turbine
  workingSetTemplate(plan, preActions, "803", sides.south, sides.north, nil, {
    {name = "PRC", i = "Bio Fuel", size = 1, maxInproc = 16}
  })

  -- pulverizer
  heterogeneousWorkingSetTemplate(plan, preActions, "pulverizer", "803", sides.up, sides.north, nil, 27, nil, {
    {o = "Osmium Dust",                  size = 256, i = "Osmium Ingot"        },
    {o = "Pulverized Iron",              size = 256, i = "Iron Ore"            },
    {o = "Pulverized Copper",            size = 256, i = "Copper Ingot"        },
    {o = "Pulverized Tin",               size = 256, i = "Tin Ingot"           },
    {o = "Pulverized Aluminum",          size = 256, i = "Aluminum Ingot"      },
    {o = "Pulverized Lead",              size = 256, i = "Lead Ingot"          },
    {o = "Pulverized Silver",            size = 256, i = "Silver Ingot"        },
    {o = "Pulverized Nickel",            size = 256, i = "Nickel Ingot"        },
    {o = "Ardite Ore Dust",              size = 256, i = "Ardite Ingot"        },
    {o = "Cobalt Ore Dust",              size = 256, i = "Cobalt Ingot"        },
    {o = "Boron Dust",                   size = 256, i = "Boron Ingot"         },
    {o = "Lithium Dust",                 size = 256, i = "Lithium Ingot"       },
    {o = "Magnesium Dust",               size = 256, i = "Magnesium Ingot"     },
    {o = "Thorium Dust",                 size = 256, i = "Thorium Ingot"       },
    {o = "Uranium Grit",                 size = 256, i = "Uranium Ingot"       },
    {o = "Clay;minecraft:clay_ball",     size = 256, i = "Clay;minecraft:clay" },
    {o = "Black Pepper",                 size = 64,  i = "Peppercorn"          },
    {o = "Pulverized Gold",              size = 256, i = "Gold Ore"            },
    {o = "Cinnabar",                     size = 16,  i = "Gold Ore"            },
    {o = "Ground Cinnamon",              size = 64,  i = "Cinnamon"            },
    {o = "Flour;harvestcraft:flouritem", size = 64,  i = "Wheat"               }
  })

  -- mekCrusher
  slottedTemplate(plan, preActions, "mekCrusher", "803", sides.west, sides.north, {1}, nil, {
    {o = "Bio Fuel", i = "Wheat",       size = 256, maxInproc = 16},
    {o = "Gravel",   i = "Cobblestone", size = 256, maxInproc = 16}
  })

  -- furnace
  heterogeneousWorkingSetTemplate(plan, preActions, "furnace", "eda", sides.up, sides.south, nil, 27, nil, {
    {o = "Black Quartz",                       size = 256, i = "Crushed Black Quartz"              },
    {o = "Charcoal",                           size = 256, i = "Birch Wood"                        },
    {o = "Glass",                              size = 256, i = "Sand"                              },
    {o = "Stone",                              size = 256, i = "Cobblestone"                       },
    {o = "Graphite Ingot",                     size = 256, i = "Charcoal"                          },
    {o = "Gold Ingot",                         size = 256, i = "Pulverized Gold"                   },
    {o = "Iron Ingot",                         size = 256, i = "Pulverized Iron"                   },
    {o = "Plastic",                            size = 64 , i = "Dry Rubber"                        },
    {o = "Bread",                              size = 16 , i = "Flour;appliedenergistics2:material"},
    {o = "Brick",                              size = 64 , i = "Clay;minecraft:clay_ball"          },
    {o = "Nether Brick;minecraft:netherbrick", size = 64 , i = "Netherrack"                        }
  })

  -- sandInduction
  heterogeneousWorkingSetTemplate(plan, preActions, "sandInduction", "ebb", sides.up, sides.west, {["Sand"] = 64}, 26, nil, {
    {o = "Osmium Ingot",    size = 256, i = "Osmium Ore"   },
    {o = "Copper Ingot",    size = 256, i = "Copper Ore"   },
    {o = "Tin Ingot",       size = 256, i = "Tin Ore"      },
    {o = "Aluminum Ingot",  size = 256, i = "Aluminum Ore" },
    {o = "Lead Ingot",      size = 256, i = "Lead Ore"     },
    {o = "Silver Ingot",    size = 256, i = "Silver Ore"   },
    {o = "Ardite Ingot",    size = 256, i = "Ardite Ore"   },
    {o = "Cobalt Ingot",    size = 256, i = "Cobalt Ore"   },
    {o = "Boron Ingot",     size = 256, i = "Boron Ore"    },
    {o = "Lithium Ingot",   size = 256, i = "Lithium Ore"  },
    {o = "Magnesium Ingot", size = 256, i = "Magnesium Ore"},
    {o = "Thorium Ingot",   size = 256, i = "Thorium Ore"  },
    {o = "Uranium Ingot",   size = 256, i = "Uranium Ore"  }
  })

  -- generalInduction
  slottedTemplate(plan, preActions, "generalInduction", "ebb", sides.east, sides.west, {1, 2}, nil, {
    {o = "Hardened Glass", size = 64, maxInproc = 32, is = {
      {item = "Pulverized Lead",     size = 1},
      {item = "Pulverized Obsidian", size = 4}
    }},
    {o = "Rich Slag", size = 16, maxInproc = 32, is = {
      {item = "Pulverized Lead", size = 1},
      {item = "Hardened Glass", size = 2}
    }},
    {o = "Platinum Ingot", size = 16, maxInproc = 16, is = {
      {item = "Nickel Ore", size = 1},
      {item = "Cinnabar", size = 1}
    }},
    {o = "Nickel Ingot", size = 256, maxInproc = 16, is = {
      {item = "Nickel Ore", size = 1},
      {item = "Cinnabar", size = 1}
    }},
    {o = "Black Iron Ingot", size = 64, maxInproc = 16, is = {
      {item = "Block of Invar", size = 1},
      {item = "Tough Alloy", size = 1}
    }},
    {o = "Aluminum Brass Ingot", size = 16, maxInproc = 16, is = {
      {item = "Aluminum Ingot", size = 3},
      {item = "Copper Ingot", size = 1}
    }},
    {o = "Industrial Machine Chassis", size = 4, maxInproc = 4, is = {
      {item = "Simple Machine Chassis", size = 1},
      {item = "Industrial Dye Blend", size = 1}
    }},
    {o = "Signalum Cell Frame (Full)", size = 4, maxInproc = 40, is = {
      {item = "Signalum Cell Frame (Empty)", size = 1},
      {item = "Block of Redstone", size = 40}
    }}
  })

  -- manufactory
  slottedTemplate(plan, preActions, "manufactory", "eda", sides.down, sides.south, {1}, nil, {
    {o = "Sand",                               i = "Cobblestone",           size = 256, maxInproc = 32},
    {o = "Niter",                              i = "Sandstone",             size = 256, maxInproc = 32},
    {o = "Flint",                              i = "Gravel",                size = 256, maxInproc = 32},
    {o = "Silicon Ingot",                      i = "Sand",                  size = 256, maxInproc = 32},
    {o = "Pulverized Coal",                    i = "Coal",                  size = 256, maxInproc = 32},
    {o = "Pulverized Obsidian",                i = "Obsidian",              size = 256, maxInproc = 32},
    {o = "Pulverized Charcoal",                i = "Charcoal",              size = 16,  maxInproc = 32},
    {o = "Certus Quartz Dust",                 i = "Certus Quartz Crystal", size = 16,  maxInproc = 32},
    {o = "Nether Quartz Dust",                 i = "Nether Quartz",         size = 16,  maxInproc = 32},
    {o = "Lapis Lazuli Dust",                  i = "Lapis Lazuli",          size = 16,  maxInproc = 32},
    {o = "Diamond Dust",                       i = "Diamond",               size = 16,  maxInproc = 32},
    {o = "Graphite Dust",                      i = "Graphite Ingot",        size = 64,  maxInproc = 32},
    {o = "Flour;appliedenergistics2:material", i = "Wheat",                 size = 16,  maxInproc = 32},
    {o = "Clay Dust",                          i = "Clay;minecraft:clay",   size = 64,  maxInproc = 32}
  })

  -- sawmill
  slottedTemplate(plan, preActions, "sawmill", "eda", sides.west, sides.south, {1}, nil, {
    {o = "Birch Wood Planks", i = "Birch Wood", size = 256, maxInproc = 16},
    {o = "Sawdust",           i = "Birch Wood", size = 256, maxInproc = 16}
  })

  -- cobble/obs/cactus/sugarCanes/netherWarts/mushroomRed/mushroomBrown/pumpkin/melon/freshWater/snowball/basalt
  heterogeneousInputlessTemplate(plan, preActions, "624", sides.west, sides.south, 256)

  -- analogCrafter
  slottedTemplate(plan, preActions, "analogCrafter", "303", sides.south, sides.north, {1, 2, 3, 4, 5, 6, 7, 8, 9}, nil, {
    {o = "Sandstone", size = 256, maxInproc = 16, is = {
      {item = "Sand", size = 4, slots = {1, 2, 4, 5}}
    }},
    {o = "Block of Quartz", size = 16, maxInproc = 16, is = {
      {item = "Nether Quartz", size = 4, slots = {1, 2, 4, 5}}
    }},
    {o = "Block of Black Quartz", size = 4, maxInproc = 4, is = {
      {item = "Black Quartz", size = 4, slots = {1, 2, 4, 5}}
    }},
    {o = "Crafting Table", size = 64, maxInproc = 16, is = {
      {item = "Birch Wood Planks", size = 4, slots = {1, 2, 4, 5}}
    }},
    {o = "Raw Carbon Fibre", size = 16, maxInproc = 16, is = {
      {item = "Pulverized Coal", size = 4, slots = {1, 2, 4, 5}}
    }},
    {o = "Raw Carbon Mesh", size = 16, maxInproc = 16, is = {
      {item = "Raw Carbon Fibre", size = 2, slots = {1, 2}}
    }},
    {o = "Compressed Cobblestone", size = 64, maxInproc = 7, is = {
      {item = "Cobblestone", size = 9, slots = {1, 2, 3, 4, 5, 6, 7, 8, 9}}
    }},
    {o = "Slime Block", size = 64, maxInproc = 7, is = {
      {item = "Slimeball", size = 9, slots = {1, 2, 3, 4, 5, 6, 7, 8, 9}}
    }},
    {o = "Osmium Block", size = 4, maxInproc = 4, is = {
      {item = "Osmium Ingot", size = 9, slots = {1, 2, 3, 4, 5, 6, 7, 8, 9}}
    }},
    {o = "Enori Crystal Block", size = 4, maxInproc = 4, is = {
      {item = "Enori Crystal", size = 9, slots = {1, 2, 3, 4, 5, 6, 7, 8, 9}}
    }},
    {o = "Palis Crystal Block", size = 4, maxInproc = 4, is = {
      {item = "Palis Crystal", size = 9, slots = {1, 2, 3, 4, 5, 6, 7, 8, 9}}
    }},
    {o = "Restonia Crystal Block", size = 4, maxInproc = 4, is = {
      {item = "Restonia Crystal", size = 9, slots = {1, 2, 3, 4, 5, 6, 7, 8, 9}}
    }},
    {o = "Void Crystal Block", size = 4, maxInproc = 4, is = {
      {item = "Void Crystal", size = 9, slots = {1, 2, 3, 4, 5, 6, 7, 8, 9}}
    }},
    {o = "Diamatine Crystal Block", size = 4, maxInproc = 4, is = {
      {item = "Diamatine Crystal", size = 9, slots = {1, 2, 3, 4, 5, 6, 7, 8, 9}}
    }},
    {o = "Emeradic Crystal Block", size = 4, maxInproc = 4, is = {
      {item = "Emeradic Crystal", size = 9, slots = {1, 2, 3, 4, 5, 6, 7, 8, 9}}
    }},
    {o = "Block of Black Iron", size = 4, maxInproc = 4, is = {
      {item = "Black Iron Ingot", size = 9, slots = {1, 2, 3, 4, 5, 6, 7, 8, 9}}
    }},
    {o = "Block of Electrum", size = 4, maxInproc = 4, is = {
      {item = "Electrum Ingot", size = 9, slots = {1, 2, 3, 4, 5, 6, 7, 8, 9}}
    }},
    {o = "Block of Redstone", size = 40, maxInproc = 7, is = {
      {item = "Redstone", size = 9, slots = {1, 2, 3, 4, 5, 6, 7, 8, 9}}
    }},
    {o = "Block of Lead", size = 16, maxInproc = 7, is = {
      {item = "Lead Ingot", size = 9, slots = {1, 2, 3, 4, 5, 6, 7, 8, 9}}
    }},
    {o = "Block of Tin", size = 16, maxInproc = 7, is = {
      {item = "Tin Ingot", size = 9, slots = {1, 2, 3, 4, 5, 6, 7, 8, 9}}
    }},
    {o = "Bone Block", size = 4, maxInproc = 4, is = {
      {item = "Bone Meal", size = 9, slots = {1, 2, 3, 4, 5, 6, 7, 8, 9}}
    }},
    {o = "Dry Rubber", size = 64, maxInproc = 7, is = {
      {item = "Tiny Dry Rubber", size = 9, slots = {1, 2, 3, 4, 5, 6, 7, 8, 9}}
    }},
    {o = "Block of Invar", size = 8, maxInproc = 7, is = {
      {item = "Invar Ingot", size = 9, slots = {1, 2, 3, 4, 5, 6, 7, 8, 9}}
    }},
    {o = "Button", i = "Stone", size = 64, maxInproc = 16},
    {o = "Cake", size = 16, maxInproc = 16, is = {
      {item = "Soy Milk",    size = 3, slots = {7, 8, 9}},
      {item = "Sugar",       size = 2, slots = {4, 6}},
      {item = "Raw Tofeeg",  size = 1, slots = {5}},
      {item = "Wheat",       size = 3, slots = {1, 2, 3}}
    }},
    {o = "Redstone Torch", size = 64, maxInproc = 16, is = {
      {item = "Stick",    size = 1, slots = {1}},
      {item = "Redstone", size = 1, slots = {4}}
    }},
    {o = "Lever", size = 64, maxInproc = 16, is = {
      {item = "Stick",    size = 1, slots = {4}},
      {item = "Cobblestone", size = 1, slots = {1}}
    }},
    {o = "Mixed Metal Ingot", size = 16, maxInproc = 16, is = {
      {item = "Tin Plate", size = 3, slots = {1, 2, 3}},
      {item = "Bronze Plate", size = 3, slots = {4, 5, 6}},
      {item = "Iron Plate", size = 3, slots = {7, 8, 9}}
    }},
    {o = "Advanced Control Circuit", size = 16, maxInproc = 16, is = {
      {item = "Redstone", size = 4, slots = {1, 3, 7, 9}},
      {item = "Enriched Alloy", size = 4, slots = {2, 4, 6, 8}},
      {item = "Basic Control Circuit", size = 1, slots = {5}}
    }},
    {o = "Machine Case", size = 16, maxInproc = 16, is = {
      {item = "Reinforced Stone", size = 4, slots = {1, 3, 7, 9}},
      {item = "Plastic", size = 4, slots = {2, 4, 6, 8}},
      {item = "Advanced Machine Casing", size = 1, slots = {5}}
    }},
    {o = "Advanced Machine Casing", size = 16, maxInproc = 16, is = {
      {item = "Steel Plate", size = 4, slots = {1, 3, 7, 9}},
      {item = "Carbon Plate", size = 2, slots = {2, 8}},
      {item = "Advanced Alloy", size = 2, slots = {4, 6}},
      {item = "Basic Machine Casing", size = 1, slots = {5}}
    }},
    {o = "Basic Coil", size = 16, maxInproc = 16, is = {
      {item = "Enori Crystal", size = 2, slots = {1, 9}},
      {item = "Aluminium Wire", size = 4, slots = {2, 4, 6, 8}},
      {item = "Impregnated Stick", size = 3, slots = {3, 5, 7}}
    }},
    {o = "Advanced Coil", size = 16, maxInproc = 16, is = {
      {item = "Basic Coil", size = 1, slots = {5}},
      {item = "Gold Cable", size = 4, slots = {2, 4, 6, 8}},
      {item = "Impregnated Stick", size = 2, slots = {3, 7}}
    }},
    {o = "1k ME Storage Component", size = 3, maxInproc = 16, is = {
      {item = "Logic Processor", size = 1, slots = {5}},
      {item = "Pure Certus Quartz Crystal", size = 4, slots = {2, 4, 6, 8}},
      {item = "Redstone", size = 4, slots = {1, 3, 7, 9}}
    }},
    {o = "4k ME Storage Component", size = 3, maxInproc = 16, is = {
      {item = "Quartz Glass", size = 1, slots = {5}},
      {item = "Calculation Processor", size = 1, slots = {8}},
      {item = "1k ME Storage Component", size = 3, slots = {2, 4, 6}},
      {item = "Redstone", size = 4, slots = {1, 3, 7, 9}}
    }},
    {o = "16k ME Storage Component", size = 3, maxInproc = 16, is = {
      {item = "Quartz Glass", size = 1, slots = {5}},
      {item = "Calculation Processor", size = 1, slots = {8}},
      {item = "4k ME Storage Component", size = 3, slots = {2, 4, 6}},
      {item = "Glowstone Dust", size = 4, slots = {1, 3, 7, 9}}
    }},
    {o = "64k ME Storage Component", size = 3, maxInproc = 16, is = {
      {item = "Quartz Glass", size = 1, slots = {5}},
      {item = "Calculation Processor", size = 1, slots = {8}},
      {item = "16k ME Storage Component", size = 3, slots = {2, 4, 6}},
      {item = "Glowstone Dust", size = 4, slots = {1, 3, 7, 9}}
    }},
    {o = "Steel Mechanical Component", size = 16, maxInproc = 16, is = {
      {item = "Steel Plate", size = 4, slots = {1, 3, 7, 9}},
      {item = "Copper Ingot", size = 1, slots = {5}}
    }},
    {o = "Iron Mechanical Component", size = 16, maxInproc = 16, is = {
      {item = "Iron Plate", size = 4, slots = {1, 3, 7, 9}},
      {item = "Copper Ingot", size = 1, slots = {5}}
    }},
    {o = "Device Frame", size = 16, maxInproc = 16, is = {
      {item = "Tin Ingot", size = 4, slots = {1, 3, 7, 9}},
      {item = "Glass", size = 4, slots = {2, 4, 6, 8}},
      {item = "Copper Gear", size = 1, slots = {5}}
    }},
    {o = "Coil", size = 16, maxInproc = 8, is = {
      {item = "Copper Cable", size = 8, slots = {1, 2, 3, 4, 6, 7, 8, 9}},
      {item = "Iron Ingot", size = 1, slots = {5}}
    }},
    {o = "Electric Motor", size = 16, maxInproc = 16, is = {
      {item = "Coil", size = 2, slots = {4, 6}},
      {item = "Tin Item Casing", size = 2, slots = {2, 8}},
      {item = "Iron Ingot", size = 1, slots = {5}}
    }},
    {o = "Machine Block", size = 16, maxInproc = 16, is = {
      {item = "Polished Stone", size = 4, slots = {1, 3, 7, 9}},
      {item = "Reinforced Stone", size = 4, slots = {2, 4, 6, 8}},
      {item = "Basic Machine Casing", size = 1, slots = {5}}
    }},
    {o = "Redstone Servo", size = 16, maxInproc = 16, is = {
      {item = "Redstone", size = 2, slots = {2, 8}},
      {item = "Iron Ingot", size = 1, slots = {5}}
    }},
    {o = "Hopper", size = 16, maxInproc = 12, is = {
      {item = "Aluminum Plate", size = 5, slots = {2, 4, 6, 7, 9}},
      {item = "Chest", size = 1, slots = {5}}
    }},
    {o = "Infinity Bimetal Gear", size = 16, maxInproc = 16, is = {
      {item = "Iron Ingot", size = 4, slots = {2, 4, 6, 8}},
      {item = "Iron Nugget", size = 4, slots = {1, 3, 7, 9}},
      {item = "Grains of Infinity", size = 1, slots = {5}}
    }},
    {o = "Pulsating Crystal", size = 16, maxInproc = 8, is = {
      {item = "Pulsating Iron Nugget", size = 8, slots = {1, 2, 3, 4, 6, 7, 8, 9}},
      {item = "Diamond", size = 1, slots = {5}}
    }},
    {o = "Analog Crafter", size = 4, maxInproc = 8, is = {
      {item = "Crafting Table", size = 8, slots = {1, 2, 3, 4, 6, 7, 8, 9}},
      {item = "Lever", size = 1, slots = {5}}
    }},
    {o = "Redstone Gear", size = 16, maxInproc = 16, is = {
      {item = "Redstone Torch", size = 4, slots = {2, 4, 6, 8}},
      {item = "Birch Wood Planks", size = 1, slots = {5}}
    }},
    {o = "Red Nether Brick", size = 4, maxInproc = 16, is = {
      {item = "Nether Brick;minecraft:netherbrick", size = 2, slots = {1, 5}},
      {item = "Nether Wart", size = 2, slots = {2, 4}}
    }},
    {o = "Terrestrial Artifact", size = 16, maxInproc = 16, is = {
      {item = "Empowered Restonia Crystal", size = 4, slots = {1, 3, 7, 9}},
      {item = "Empowered Emeradic Crystal", size = 1, slots = {8}},
      {item = "Empowered Palis Crystal", size = 1, slots = {6}},
      {item = "Empowered Void Crystal", size = 1, slots = {4}},
      {item = "Empowered Enori Crystal", size = 1, slots = {2}},
      {item = "Empowered Diamatine Crystal", size = 1, slots = {5}}
    }},
    {o = "Connector;environmentaltech:connector", size = 16, maxInproc = 16, is = {
      {item = "Signalum Ingot", size = 4, slots = {1, 3, 7, 9}},
      {item = "Block of Tin", size = 4, slots = {2, 4, 6, 8}},
      {item = "Aluminum Brass Ingot", size = 1, slots = {5}}
    }},
    {o = "Interconnect", size = 16, maxInproc = 16, is = {
      {item = "Connector;environmentaltech:connector", size = 4, slots = {2, 4, 6, 8}},
      {item = "Black Concrete", size = 1, slots = {5}}
    }},
    {o = "LV Capacitor", size = 4, maxInproc = 4, is = {
      {item = "Treated Wood Planks", size = 2, slots = {1, 3}},
      {item = "Copper Ingot", size = 2, slots = {4, 6}},
      {item = "Iron Ingot", size = 3, slots = {7, 8, 9}},
      {item = "Lead Ingot", size = 1, slots = {5}},
      {item = "Redstone", size = 1, slots = {2}}
    }},
    {o = "MV Capacitor", size = 4, maxInproc = 4, is = {
      {item = "Treated Wood Planks", size = 2, slots = {1, 3}},
      {item = "Electrum Ingot", size = 2, slots = {4, 6}},
      {item = "Iron Ingot", size = 3, slots = {7, 8, 9}},
      {item = "LV Capacitor", size = 1, slots = {5}},
      {item = "Block of Redstone", size = 1, slots = {2}}
    }},
    {o = "HV Capacitor", size = 4, maxInproc = 4, is = {
      {item = "Treated Wood Planks", size = 2, slots = {1, 3}},
      {item = "Block of Lead", size = 2, slots = {4, 6}},
      {item = "Steel Ingot", size = 3, slots = {7, 8, 9}},
      {item = "MV Capacitor", size = 1, slots = {5}},
      {item = "Block of Redstone", size = 1, slots = {2}}
    }},
    {o = "Reinforced Cell Frame (Empty)", size = 4, maxInproc = 4, is = {
      {item = "Silver Gear", size = 2, slots = {7, 9}},
      {item = "Fluxed Electrum Plate", size = 2, slots = {4, 6}},
      {item = "Flux Crystal", size = 2, slots = {1, 3}},
      {item = "Redstone Conductance Coil", size = 1, slots = {8}},
      {item = "Hardened Cell Frame", size = 1, slots = {5}},
      {item = "Electrum Large Plate", size = 1, slots = {2}}
    }},
    {o = "Simple Machine Chassis", size = 4, maxInproc = 4, is = {
      {item = "Dark Iron Bars", size = 2, slots = {4, 6}},
      {item = "Infinity Bimetal Gear", size = 2, slots = {2, 8}},
      {item = "Hardened Cell Frame", size = 1, slots = {5}},
      {item = "Titanium Aluminide Plate", size = 4, slots = {1, 3, 7, 9}}
    }},
    {o = "Signalum Cell Frame (Empty)", size = 4, maxInproc = 4, is = {
      {item = "Rosin", size = 2, slots = {4, 6}},
      {item = "Reinforced Cell Frame (Full)", size = 1, slots = {5}},
      {item = "Rich Slag", size = 1, slots = {2}},
      {item = "Cinnabar", size = 1, slots = {8}},
      {item = "Signalum Plate", size = 4, slots = {1, 3, 7, 9}}
    }}
  })

  -- pot
  slottedTemplate(plan, preActions, "pot", "ebb", sides.down, sides.west, {1, 2, 3, 4, 5, 6, 7, 8}, nil, {
    {o = "Cheese", size = 16, maxInproc = 16, is = {
      {item = "Salt", size = 1},
      {item = "Soy Milk", size = 1}
    }},
    {o = "Corned Beef", size = 16, maxInproc = 16, is = {
      {item = "Raw Tofeak", size = 1},
      {item = "Salt", size = 1},
      {item = "Sugar", size = 1},
      {item = "Cinnamon", size = 1},
      {item = "Mustard Seeds", size = 1},
      {item = "Peppercorn", size = 1},
      {item = "Spice Leaf", size = 1},
      {item = "Ginger", size = 1}
    }},
    {o = "Mango Chutney", size = 16, maxInproc = 16, is = {
      {item = "Mango", size = 1},
      {item = "Spice Leaf", size = 1},
      {item = "Mustard Seeds", size = 1},
      {item = "Cooking Oil", size = 1}
    }},
    {o = "Plain Yogurt", size = 16, maxInproc = 16, is = {
      {item = "Soy Milk", size = 1}
    }},
    {o = "Chicken Curry", size = 16, maxInproc = 16, is = {
      {item = "Coconut", size = 1},
      {item = "Plain Yogurt", size = 1},
      {item = "Raw Toficken", size = 1},
      {item = "Ginger", size = 1},
      {item = "Chili Pepper", size = 1},
      {item = "Rice", size = 1},
      {item = "Ground Cinnamon", size = 1},
      {item = "Garlic", size = 1}
    }}
  })

  -- juicer
  slottedTemplate(plan, preActions, "juicer", "c47", sides.down, sides.east, {1, 2, 3, 4, 5, 6, 7, 8}, nil, {
    {o = "Ketchup", size = 16, maxInproc = 16, is = {
      {item = "Tomato", size = 1}
    }},
    {o = "Cooking Oil", size = 16, maxInproc = 16, is = {
      {item = "Tea Leaf", size = 1}
    }},
    {o = "Soy Sauce", size = 16, maxInproc = 16, is = {
      {item = "Soybean", size = 1},
      {item = "Salt", size = 1},
      {item = "Fresh Water", size = 1}
    }},
    {o = "Soy Sauce", size = 16, maxInproc = 16, is = {
      {item = "Soybean", size = 1},
      {item = "Salt", size = 1},
      {item = "Fresh Water", size = 1}
    }}
  })

  -- skillet
  slottedTemplate(plan, preActions, "skillet", "303", sides.west, sides.north, {1, 2, 3, 4, 5, 6, 7, 8}, nil, {
    {o = "Corned Beef Hash", size = 16, maxInproc = 16, is = {
      {item = "Corned Beef", size = 1},
      {item = "Onion", size = 1},
      {item = "Bellpepper", size = 1},
      {item = "Potato", size = 1},
      {item = "Raw Tofeeg", size = 1},
      {item = "Butter", size = 1},
      {item = "Cheese", size = 1}
    }},
    {o = "Corned Beef Breakfast", size = 256, maxInproc = 16, is = {
      {item = "Corned Beef Hash", size = 1},
      {item = "Raw Tofeeg", size = 1},
      {item = "Toast", size = 1},
      {item = "Ketchup", size = 1},
      {item = "Soy Milk", size = 1}
    }},
    {o = "Naan", size = 16, maxInproc = 16, is = {
      {item = "Dough", size = 1},
      {item = "Onion", size = 1},
      {item = "Cooking Oil", size = 1}
    }}
  })

  -- teCharger
  slottedTemplate(plan, preActions, "teCharger", "ebb", sides.south, sides.west, {1}, nil, {
    {o = "Fluxed Phyto-Gro", size = 16, maxInproc = 32, i = "Rich Phyto-Gro"}
  })

  -- phyto
  heterogeneousWorkingSetTemplate(plan, preActions, "phyto", "c47", sides.south, sides.east, {["Fluxed Phyto-Gro"] = 64}, 16, nil, {
    {o = "Peppercorn",            size = 64, i = "Peppercorn Sapling",  allowBackup = true},
    {o = "Peppercorn Sapling",    size = 16, i = "Peppercorn Sapling",  allowBackup = true},
    {o = "§eInferium Essence",    size = 64, i = inferiumSeeds,         allowBackup = true},
    {o = inferiumSeeds,           size = 16, i = inferiumSeeds,         allowBackup = true},
    {o = "Nature Essence",        size = 64, i = "Nature Seeds",        allowBackup = true},
    {o = "Nature Seeds",          size = 16, i = "Nature Seeds",        allowBackup = true},
    {o = "Sulfur Essence",        size = 64, i = "Sulfur Seeds",        allowBackup = true},
    {o = "Sulfur Seeds",          size = 16, i = "Sulfur Seeds",        allowBackup = true},
    {o = "Nether Quartz Essence", size = 64, i = "Nether Quartz Seeds", allowBackup = true},
    {o = "Nether Quartz Seeds",   size = 16, i = "Nether Quartz Seeds", allowBackup = true},
    {o = "Experience Essence",    size = 64, i = "Experience Seeds",    allowBackup = true},
    {o = "Experience Seeds",      size = 16, i = "Experience Seeds",    allowBackup = true},
    {o = "Enderman Essence",      size = 64, i = "Enderman Seeds",      allowBackup = true},
    {o = "Enderman Seeds",        size = 16, i = "Enderman Seeds",      allowBackup = true},
    {o = "Mango",                 size = 64, i = "Mango Sapling",       allowBackup = true},
    {o = "Mango Sapling",         size = 16, i = "Mango Sapling",       allowBackup = true},
    {o = "Coconut",               size = 64, i = "Coconut Sapling",     allowBackup = true},
    {o = "Coconut Sapling",       size = 16, i = "Coconut Sapling",     allowBackup = true},
    {o = "Fiery Ingot Essence",   size = 64, i = "Fiery Ingot Seeds",   allowBackup = true},
    {o = "Fiery Ingot Seeds",     size = 16, i = "Fiery Ingot Seeds",   allowBackup = true},
    {o = "Emerald Essence",       size = 64, i = "Emerald Seeds",       allowBackup = true},
    {o = "Emerald Seeds",         size = 16, i = "Emerald Seeds",       allowBackup = true},
    {o = "Redstone Essence",      size = 64, i = "Redstone Seeds",      allowBackup = true},
    {o = "Redstone Seeds",        size = 16, i = "Redstone Seeds",      allowBackup = true}
  })

  -- crafterE
  heterogeneousWorkingSetTemplate(plan, preActions, "crafterE", "c47", sides.west, sides.east, nil, 25, nil, {
    {o = "§aPrudentium Essence", size = 64, is = {
      {item = "§eInferium Essence", size = 4}
    }},
    {o = "§6Intermedium Essence", size = 64, is = {
      {item = "§aPrudentium Essence", size = 4}
    }},
    {o = "§bSuperium Essence", size = 64, is = {
      {item = "§6Intermedium Essence", size = 4}
    }},
    {o = "§cSupremium Essence", size = 64, is = {
      {item = "§bSuperium Essence", size = 4}
    }},
    {o = "§5Insanium Essence", size = 64, is = {
      {item = "§cSupremium Essence", size = 4}
    }},
    {o = "Apple", size = 64, is = {
      {item = "Nature Essence", size = 3}
    }},
    {o = "Sulfur", size = 64, is = {
      {item = "Sulfur Essence", size = 3}
    }},
    {o = "Ender Pearl", size = 64, is = {
      {item = "Enderman Essence", size = 8}
    }}
  })

  -- teCompactor
  slottedTemplate(plan, preActions, "teCompactor", "c47", sides.up, sides.east, {1}, nil, {
    {o = "Blaze Rod",            size = 16,  maxInproc = 30, is = {{item = "Blaze Powder", size = 5}}},
    {o = "Coal Ball",            size = 16,  maxInproc = 64, is = {{item = "Pulverized Coal", size = 8}}},
    {o = "Compressed Coal Ball", size = 16,  maxInproc = 16, i = "Coal Ball"},
    {o = "Coal Chunk",           size = 16,  maxInproc = 64, is = {{item = "Compressed Coal Ball", size = 8}}},
    {o = "Diamond",              size = 256, maxInproc = 16, i = "Coal Chunk"}
  })

  -- autoCompressor
  heterogeneousWorkingSetTemplate(plan, preActions, "autoCompressor", "0b0", sides.west, sides.east, nil, 48, nil, {
    {o = "Aluminum Ore",  is = {{item = "Aluminium Ore Piece", size = 4}}, size = 256},
    {o = "Osmium Ore",    is = {{item = "Osmium Ore Piece",    size = 4}}, size = 256},
    {o = "Gold Ore",      is = {{item = "Gold Ore Piece",      size = 4}}, size = 256},
    {o = "Iron Ore",      is = {{item = "Iron Ore Piece",      size = 4}}, size = 256},
    {o = "Copper Ore",    is = {{item = "Copper Ore Piece",    size = 4}}, size = 256},
    {o = "Tin Ore",       is = {{item = "Tin Ore Piece",       size = 4}}, size = 256},
    {o = "Lead Ore",      is = {{item = "Lead Ore Piece",      size = 4}}, size = 256},
    {o = "Silver Ore",    is = {{item = "Silver Ore Piece",    size = 4}}, size = 256},
    {o = "Nickel Ore",    is = {{item = "Nickel Ore Piece",    size = 4}}, size = 256},
    {o = "Ardite Ore",    is = {{item = "Ardite Ore Piece",    size = 4}}, size = 256},
    {o = "Cobalt Ore",    is = {{item = "Cobalt Ore Piece",    size = 4}}, size = 256},
    {o = "Boron Ore",     is = {{item = "Boron Ore Piece",     size = 4}}, size = 256},
    {o = "Lithium Ore",   is = {{item = "Lithium Ore Piece",   size = 4}}, size = 256},
    {o = "Magnesium Ore", is = {{item = "Magnesium Ore Piece", size = 4}}, size = 256},
    {o = "Thorium Ore",   is = {{item = "Thorium Ore Piece",   size = 4}}, size = 256},
    {o = "Uranium Ore",   is = {{item = "Uranium Ore Piece",   size = 4}}, size = 256}
  })

  -- planter
  heterogeneousWorkingSetTemplate(plan, preActions, "planter", "e54", sides.west, sides.down, nil, 8, nil, {
    {o = "Birch Wood",       i = "Birch Sapling",  size = 256, allowBackup = true},
    {o = "Wheat",            i = "Seeds",          size = 256, allowBackup = true},
    {o = "Seeds",            i = "Seeds",          size = 256, allowBackup = true},
    {o = "Carrot",           i = "Carrot",         size = 256, allowBackup = true},
    {o = "Cotton",           i = "Cotton",         size = 256, allowBackup = true},
    {o = "Soybean",          i = "Soybean",        size = 256, allowBackup = true},
    {o = "Beetroot",         i = "Beetroot Seeds", size = 256, allowBackup = true},
    {o = "Beetroot Seeds",   i = "Beetroot Seeds", size = 256, allowBackup = true},
    {o = "Tomato",           i = "Tomato",         size = 256, allowBackup = true},
    {o = "Coffee Beans",     i = "Coffee Beans",   size = 256, allowBackup = true},
    {o = "Potato",           i = "Potato",         size = 256, allowBackup = true},
    {o = "Tea Leaf",         i = "Tea Leaf",       size = 256, allowBackup = true},
    {o = "Mustard Seeds",    i = "Mustard Seeds",  size = 256, allowBackup = true},
    {o = "Spice Leaf",       i = "Spice Leaf",     size = 256, allowBackup = true},
    {o = "Ginger",           i = "Ginger",         size = 256, allowBackup = true},
    {o = "Onion",            i = "Onion",          size = 256, allowBackup = true},
    {o = "Bellpepper",       i = "Bellpepper",     size = 256, allowBackup = true},
    {o = "Garlic",           i = "Garlic",         size = 256, allowBackup = true},
    {o = "Rice",             i = "Rice",           size = 256, allowBackup = true},
    {o = "Chili Pepper",     i = "Chili Pepper",   size = 256, allowBackup = true}
  })

  -- sieveDiamond
  slottedTemplate(plan, preActions, "sieveDiamond", "eda", sides.north, sides.south, {1}, nil, {
    {o = "Aquamarine",            i = "Sand",      size = 256,  maxInproc = 32},
    {o = "Sky Stone Dust",        i = "Dust",      size = 256,  maxInproc = 32},
    {o = "Certus Quartz Crystal", i = "Dust",      size = 256,  maxInproc = 32},
    {o = "Osmium Ore Piece",      i = "Gravel",    size = 256,  maxInproc = 32},
    {o = "Gold Ore Piece",        i = "Gravel",    size = 256,  maxInproc = 32},
    {o = "Iron Ore Piece",        i = "Gravel",    size = 256,  maxInproc = 32},
    {o = "Copper Ore Piece",      i = "Gravel",    size = 256,  maxInproc = 32},
    {o = "Tin Ore Piece",         i = "Gravel",    size = 256,  maxInproc = 32},
    {o = "Aluminium Ore Piece",   i = "Gravel",    size = 256,  maxInproc = 32},
    {o = "Lead Ore Piece",        i = "Gravel",    size = 256,  maxInproc = 32},
    {o = "Silver Ore Piece",      i = "Gravel",    size = 256,  maxInproc = 32},
    {o = "Nickel Ore Piece",      i = "Gravel",    size = 256,  maxInproc = 32},
    {o = "Crushed Black Quartz",  i = "Gravel",    size = 256,  maxInproc = 32},
    {o = "Ghast Tear",            i = "Soul Sand", size = 256,  maxInproc = 32},
    {o = "Nether Quartz",         i = "Soul Sand", size = 256,  maxInproc = 32},
    {o = "Ardite Ore Piece",      i = "Crushed Netherrack", size = 256, maxInproc = 32},
    {o = "Cobalt Ore Piece",      i = "Crushed Netherrack", size = 256, maxInproc = 32},
    {o = "Boron Ore Piece",       i = "Crushed Netherrack", size = 256, maxInproc = 32},
    {o = "Lithium Ore Piece",     i = "Crushed Netherrack", size = 256, maxInproc = 32},
    {o = "Magnesium Ore Piece",   i = "Crushed Netherrack", size = 256, maxInproc = 32},
    {o = "Thorium Ore Piece",     i = "Crushed Netherrack", size = 256, maxInproc = 32},
    {o = "Ruby",                  i = "Crushed Endstone",   size = 64,  maxInproc = 32},
    {o = "Malachite",             i = "Crushed Endstone",   size = 64,  maxInproc = 32},
    {o = "Sapphire",              i = "Crushed Endstone",   size = 64,  maxInproc = 32}
  })

  -- sieveIron
  slottedTemplate(plan, preActions, "sieveIron", "0b0", sides.north, sides.east, {1}, nil, {
    {o = "Uranium Ore Piece", i = "Crushed Endstone", size = 256, maxInproc = 32}
  })

  -- autoHammer
  slottedTemplate(plan, preActions, "autoHammer", "e54", sides.east, sides.down, {1}, nil, {
    {o = "Gravel",             i = "Cobblestone", size = 256, maxInproc = 32},
    {o = "Dust",               i = "Sand",        size = 256, maxInproc = 32},
    {o = "Crushed Netherrack", i = "Netherrack",  size = 256, maxInproc = 32},
    {o = "Crushed Endstone",   i = "End Stone",   size = 256, maxInproc = 32}
  })
  heterogeneousWorkingSetTemplate(plan, preActions, "autoHammer", "e54", sides.north, sides.down, {
    ["Diamond"] = 2,
    ["Stick"] = 2
  }, 0, nil, {})

  -- alloyFurnace
  slottedTemplate(plan, preActions, "alloyFurnace", "128", sides.down, sides.west, {1, 2}, nil, {
    {o = "Steel Ingot", size = 256, maxInproc = 16, is = {
      {item = "Pulverized Iron", size = 1},
      {item = "Graphite Ingot", size = 1}
    }},
    {o = "Bronze Ingot", size = 256, maxInproc = 16, is = {
      {item = "Copper Ingot", size = 3},
      {item = "Tin Ingot", size = 1}
    }},
    {o = "Electrical Steel Ingot", size = 256, maxInproc = 16, is = {
      {item = "Steel Ingot", size = 1},
      {item = "Silicon Ingot", size = 1}
    }},
    {o = "Tough Alloy", size = 256, maxInproc = 16, is = {
      {item = "Ferroboron Alloy", size = 1},
      {item = "Lithium Ingot", size = 1}
    }},
    {o = "Ferroboron Alloy", size = 256, maxInproc = 16, is = {
      {item = "Steel Ingot", size = 1},
      {item = "Boron Ingot", size = 1}
    }},
    {o = "Invar Ingot", size = 64, maxInproc = 16, is = {
      {item = "Pulverized Iron", size = 2},
      {item = "Nickel Ingot", size = 1}
    }},
    {o = "Shibuichi Alloy", size = 16, maxInproc = 16, is = {
      {item = "Silver Ingot", size = 1},
      {item = "Copper Ingot", size = 3}
    }},
    {o = "Lead Platinum Alloy", size = 16, maxInproc = 16, is = {
      {item = "Platinum Ingot", size = 1},
      {item = "Lead Ingot", size = 3}
    }},
    {o = "Tin Silver Alloy", size = 16, maxInproc = 16, is = {
      {item = "Silver Ingot", size = 1},
      {item = "Tin Ingot", size = 3}
    }},
    {o = "Electrum Ingot", size = 64, maxInproc = 16, is = {
      {item = "Pulverized Gold", size = 1},
      {item = "Silver Ingot", size = 1}
    }},
    {o = "Constantan Ingot", size = 16, maxInproc = 16, is = {
      {item = "Copper Ingot", size = 1},
      {item = "Nickel Ingot", size = 1}
    }},
    {o = "Pulsating Iron Ingot", size = 16, maxInproc = 16, is = {
      {item = "Iron Ingot", size = 1},
      {item = "Ender Pearl", size = 1}
    }},
    {o = "Manyullyn Ingot", size = 16, maxInproc = 16, is = {
      {item = "Cobalt Ingot", size = 1},
      {item = "Ardite Ingot", size = 1}
    }},
    {o = "Titanium Aluminide Ingot", size = 16, maxInproc = 14, is = {
      {item = "Aluminum Ingot", size = 7},
      {item = "Titanium Ingot", size = 3}
    }},
    {o = "Dark Steel Ingot", size = 16, maxInproc = 16, is = {
      {item = "Steel Ingot", size = 1},
      {item = "Obsidian", size = 1}
    }}
  })

  -- paperCrafter
  slottedTemplate(plan, preActions, "paperCrafter", "0b0", sides.up, sides.east, {1, 2, 3, 4}, nil, {
    {o = "Paper", is = {{item = "Sawdust", size = 4, slots = {1, 2, 3, 4}}}, size = 256, maxInproc = 16}
  })

  -- teCentrifuge
  slottedTemplate(plan, preActions, "teCentrifuge", "128", sides.east, sides.west, {1}, nil, {
    {o = "Firm Tofu",   i = "Silken Tofu", size = 256, maxInproc = 48},
    {o = "Soy Milk",    i = "Silken Tofu", size = 256, maxInproc = 48},
    {o = "Silken Tofu", i = "Soybean",     size = 256, maxInproc = 48}
  })

  -- exuCrusher
  slottedTemplate(plan, preActions, "exuCrusher", "128", sides.up, sides.west, {1}, nil, {
    {o = "Rose Red", i = "Beetroot",    size = 256, maxInproc = 16},
    {o = "Gravel",   i = "Cobblestone", size = 256, maxInproc = 16}
  })

  -- dyeOddToEven
  slottedTemplate(plan, preActions, "dyeOddToEven", "128", sides.north, sides.west, {1}, nil, {
    {o = "Lapis Lazuli",   i = "Cocoa Beans",      size = 256, maxInproc = 32},
    {o = "Gray Dye",       i = "Light Gray Dye",   size = 256, maxInproc = 32},
    {o = "Orange Dye",     i = "Magenta Dye",      size = 256, maxInproc = 32},
    {o = "Ink Sac",        i = "Bone Meal",        size = 256, maxInproc = 32},
    {o = "Cyan Dye",       i = "Purple Dye",       size = 256, maxInproc = 32},
    {o = "Light Blue Dye", i = "Dandelion Yellow", size = 256, maxInproc = 32},
    {o = "Lime Dye",       i = "Pink Dye",         size = 256, maxInproc = 32}
  })

  -- dyeEvenToOdd
  slottedTemplate(plan, preActions, "dyeEvenToOdd", "303", sides.down, sides.north, {1}, nil, {
    {o = "Cocoa Beans",      i = "Cactus Green",   size = 256, maxInproc = 32},
    {o = "Purple Dye",       i = "Lapis Lazuli",   size = 256, maxInproc = 32},
    {o = "Light Gray Dye",   i = "Cyan Dye",       size = 256, maxInproc = 32},
    {o = "Pink Dye",         i = "Gray Dye",       size = 256, maxInproc = 32},
    {o = "Bone Meal",        i = "Orange Dye",     size = 256, maxInproc = 32},
    {o = "Dandelion Yellow", i = "Lime Dye",       size = 256, maxInproc = 32},
    {o = "Magenta Dye",      i = "Light Blue Dye", size = 256, maxInproc = 32}
  })

  -- idSqueezer
  slottedTemplate(plan, preActions, "idSqueezer", "303", sides.east, sides.north, {1}, nil, {
    {o = "Sugar",        i = "Sugar Canes", size = 256, maxInproc = 16},
    {o = "Cactus Green", i = "Cactus",      size = 256, maxInproc = 16}
  })

  -- arPresser
  slottedTemplate(plan, preActions, "arPresser", "e54", sides.south, sides.down, {1}, nil, {
    {o = "Stick",     i = "Birch Wood Planks", size = 256, maxInproc = 16},
    {o = "Steel Rod", i = "Steel Sheetmetal",  size = 16,  maxInproc = 16},
    {o = "Iron Rod",  i = "Iron Sheetmetal",   size = 16,  maxInproc = 16}
  })

  -- pressurizer
  slottedTemplate(plan, preActions, "pressurizer", "0b0", sides.down, sides.east, {1}, nil, {
    {o = "Iron Plate",               size = 64,  maxInproc = 16, i = "Iron Ingot"              },
    {o = "Lead Plate",               size = 64,  maxInproc = 16, i = "Lead Ingot"              },
    {o = "Aluminum Plate",           size = 64,  maxInproc = 16, i = "Aluminum Ingot"          },
    {o = "Steel Plate",              size = 64,  maxInproc = 16, i = "Steel Ingot"             },
    {o = "Bronze Plate",             size = 64,  maxInproc = 16, i = "Bronze Ingot"            },
    {o = "Tin Plate",                size = 64,  maxInproc = 16, i = "Tin Ingot"               },
    {o = "Copper Plate",             size = 64,  maxInproc = 16, i = "Copper Ingot"            },
    {o = "Invar Plate",              size = 16,  maxInproc = 16, i = "Invar Ingot"             },
    {o = "Signalum Plate",           size = 16,  maxInproc = 16, i = "Signalum Ingot"          },
    {o = "Fluxed Electrum Plate",    size = 16,  maxInproc = 16, i = "Fluxed Electrum Ingot"   },
    {o = "Uranium Plate",            size = 4,   maxInproc = 16, i = "Uranium Ingot"           },
    {o = "Titanium Aluminide Plate", size = 4,   maxInproc = 16, i = "Titanium Aluminide Ingot"},
    {o = "Fluorite",                 size = 16,  maxInproc = 16, i = "Crushed Fluorite"        },
    {o = "Coal",                     size = 256, maxInproc = 16, i = "Graphite Dust"           },
    {o = "Lapis Lazuli Plate",       size = 16,  maxInproc = 16, i = "Lapis Lazuli"            },
    {o = "Dense Iron Plate", size = 16, maxInproc = 63, is = {
      {item = "Iron Plate", size = 9}
    }},
    {o = "Dense Lapis Lazuli Plate", size = 16, maxInproc = 63, is = {
      {item = "Lapis Lazuli Plate", size = 9}
    }}
  })

  -- metalPressGear
  heterogeneousWorkingSetTemplate(plan, preActions, "metalPressGear", "624", sides.east, sides.south, nil, 27, nil, {
    {o = "Bronze Gear",       is = {{item = "Bronze Ingot",       size = 4}}, size = 16},
    {o = "Gold Gear",         is = {{item = "Gold Ingot",         size = 4}}, size = 16},
    {o = "Copper Gear",       is = {{item = "Copper Ingot",       size = 4}}, size = 16},
    {o = "Invar Gear",        is = {{item = "Invar Ingot",        size = 4}}, size = 16},
    {o = "Iron Gear",         is = {{item = "Iron Ingot",         size = 4}}, size = 16},
    {o = "Electrum Gear",     is = {{item = "Electrum Ingot",     size = 4}}, size = 16},
    {o = "Silver Gear",       is = {{item = "Silver Ingot",       size = 4}}, size = 16},
    {o = "Lumium Gear",       is = {{item = "Lumium Ingot",       size = 4}}, size = 16},
    {o = "Nickel Gear",       is = {{item = "Nickel Ingot",       size = 4}}, size = 16},
    {o = "Constantan Gear",   is = {{item = "Constantan Ingot",   size = 4}}, size = 16},
    {o = "Mana Infused Gear", is = {{item = "Mana Infused Ingot", size = 4}}, size = 16}
  })

  -- metalPressWire/metalPressRod/chiseledQuartz
  workingSetTemplate(plan, preActions, "624", sides.down, sides.south, nil, {
    {o = "Aluminium Wire",        size = 64,  maxInproc = 16, name = "metalPressWire", i = "Aluminum Ingot" },
    {o = "Steel Wire",            size = 64,  maxInproc = 16, name = "metalPressWire", i = "Steel Ingot"    },
    {o = "Copper Wire",           size = 64,  maxInproc = 16, name = "metalPressWire", i = "Copper Ingot"   },
    {o = "Ardite Tool Rod",       size = 4,   maxInproc = 16, name = "metalPressRod",  i = "Ardite Ingot"   },
    {o = "Chiseled Quartz Block", size = 16,  maxInproc = 16, name = "chiseledQuartz", i = "Block of Quartz"}
  })

  -- creosote
  slottedTemplate(plan, preActions, "creosote", "624", sides.north, sides.south, {1}, nil, {
    {i = "Coal", size = 1, maxInproc = 16}
  })

  -- pinkSlime
  heterogeneousWorkingSetTemplate(plan, preActions, "pinkSlime", "803", sides.down, sides.north, nil, 16, nil, {
    {o = "Pink Slime", i = "Slime Block", size = 512}
  })
end)

local cycleBlack = proxyCycleTemplate("black", function(plan, preActions)
  -- kekimurus
  slottedTemplate(plan, preActions, "kekimurus", "603", sides.north, sides.south, {1}, nil, {
    {i = "Cake", size = 1, maxInproc = 1}
  })

  -- conjuration
  slottedTemplate(plan, preActions, "conjuration", "603", sides.east, sides.south, {1}, nil, {
    {o = "Netherrack",      i = "Netherrack",  size = 256, maxInproc = 16, allowBackup = true},
    {o = "Mana Diamond",    i = "Diamond",     size = 16,  maxInproc = 16},
    {o = "Manasteel Ingot", i = "Iron Ingot",  size = 64,  maxInproc = 16},
    {o = "Mana Powder",     i = "Gunpowder",   size = 64,  maxInproc = 16},
    {o = "Mana Pearl",      i = "Ender Pearl", size = 64,  maxInproc = 16}
  })

  -- alchemy
  slottedTemplate(plan, preActions, "alchemy", "603", sides.west, sides.south, {1}, nil, {
    {o = "Gunpowder",      i = "Flint",    size = 256, maxInproc = 16},
    {o = "Glowstone Dust", i = "Redstone", size = 256, maxInproc = 16},
    {o = "Slimeball",      i = "Cactus",   size = 256, maxInproc = 16},
    {o = "Andesite",       i = "Stone",    size = 256, maxInproc = 16},
    {o = "Diorite",        i = "Andesite", size = 256, maxInproc = 16}
  })

  -- treatedWood
  slottedTemplate(plan, preActions, "treatedWood", "603", sides.down, sides.south, {1}, nil, {
    {o = "Treated Wood Planks", i = "Birch Wood Planks", size = 256, maxInproc = 16}
  })

  -- waterBarrel
  slottedTemplate(plan, preActions, "waterBarrel", "38c", sides.down, sides.north, {1}, nil, {
    {o = "Clay;minecraft:clay", i = "Dust", size = 64, maxInproc = 16},
    {o = "Black Concrete", i = "Black Concrete Powder", size = 16, maxInproc = 16}
  })

  -- crafterA
  heterogeneousWorkingSetTemplate(plan, preActions, "crafterA", "38c", sides.east, sides.north, nil, 26, nil, {
    {o = "Grout", size = 64, is = {
      {item = "Clay;minecraft:clay", size = 1},
      {item = "Gravel", size = 4},
      {item = "Sand", size = 4}
    }},
    {o = "Iron Sheetmetal", size = 64, is = {
      {item = "Iron Plate", size = 4}
    }},
    {o = "Lead Sheetmetal", size = 64, is = {
      {item = "Lead Plate", size = 4}
    }},
    {o = "Basic Machine Casing", size = 16, is = {
      {item = "Aluminum Plate", size = 4},
      {item = "Dense Iron Plate", size = 4}
    }},
    {o = "Reinforced Stone", size = 64, is = {
      {item = "Stone", size = 4},
      {item = "Grout", size = 4},
      {item = "Clay Dust", size = 1}
    }},
    {o = "Silicon", size = 256, is = {
      {item = "Silicon Ingot", size = 2}
    }},
    {o = "Chest", size = 64, is = {
      {item = "Birch Wood", size = 4},
      {item = "Treated Wood Planks", size = 4},
      {item = "Button", size = 1}
    }},
    {o = "Piston", size = 64, is = {
      {item = "Treated Wood Planks", size = 3},
      {item = "Compressed Cobblestone", size = 4},
      {item = "Tin Plate", size = 1},
      {item = "Redstone", size = 1}
    }}
  })

  -- crafterB
  heterogeneousWorkingSetTemplate(plan, preActions, "crafterB", "540", sides.east, sides.north, nil, 26, nil, {
    {o = "String", size = 256, is = {
      {item = "Cotton", size = 3}
    }},
    {o = "Certus Quartz Seed", size = 16, is = {
      {item = "Sand", size = 1},
      {item = "Certus Quartz Dust", size = 1}
    }},
    {o = "Nether Quartz Seed", size = 16, is = {
      {item = "Sand", size = 1},
      {item = "Nether Quartz Dust", size = 1}
    }},
    {o = "Quartz Glass", size = 16, is = {
      {item = "Glass", size = 4},
      {item = "Nether Quartz Dust", size = 5}
    }},
    {o = "Blast Brick", size = 16, is = {
      {item = "Nether Brick;minecraft:netherbrick", size = 4},
      {item = "Brick", size = 4},
      {item = "Blaze Powder", size = 1}
    }},
    {o = "Reinforced Blast Brick", size = 16, is = {
      {item = "Blast Brick", size = 1},
      {item = "Steel Plate", size = 1}
    }},
    {o = "Steel Casing", size = 16, is = {
      {item = "Reinforced Blast Brick", size = 1},
      {item = "Steel Mechanical Component", size = 2},
      {item = "Osmium Block", size = 2},
      {item = "Osmium Ingot", size = 4}
    }},
    {o = "Iron Bars", size = 16, is = {
      {item = "Iron Ingot", size = 6}
    }}
  })

  -- output
  workingSetTemplate(plan, preActions, "38c", sides.west, sides.north, function() return true end, {})

  -- mekInfuserDiamond
  slottedTemplate(plan, preActions, "mekInfuserDiamond", "38c", sides.south, sides.north, {1}, nil, {
    {o = "Reinforced Alloy",      size = 16, maxInproc = 16, i = "Enriched Alloy"     },
    {o = "Refined Obsidian Dust", size = 16, maxInproc = 16, i = "Pulverized Obsidian"}
  })
  slottedTemplate(plan, preActions, "mekInfuserDiamond", "dee", sides.north, sides.south, {1}, nil, {
    {i = "Compressed Diamond", size = 1, maxInproc = 16}
  })

  -- atomicReconstructor
  slottedTemplate(plan, preActions, "atomicReconstructor", "dee", sides.down, sides.south, {1}, nil, {
    {o = "Restonia Crystal",           i = "Redstone",              size = 64, maxInproc = 16},
    {o = "Palis Crystal",              i = "Lapis Lazuli",          size = 64, maxInproc = 16},
    {o = "Void Crystal",               i = "Coal",                  size = 64, maxInproc = 16},
    {o = "Enori Crystal",              i = "Iron Ingot",            size = 64, maxInproc = 16},
    {o = "Soul Sand",                  i = "Sand",                  size = 64, maxInproc = 16},
    {o = "Prismarine Shard",           i = "Nether Quartz",         size = 64, maxInproc = 16},
    {o = "Rhodochrosite",              i = "Ruby",                  size = 16, maxInproc = 16},
    {o = "Diamatine Crystal",          i = "Diamond",               size = 16, maxInproc = 16},
    {o = "Emeradic Crystal",           i = "Emerald",               size = 16, maxInproc = 16},
    {o = "Ethetic Green Block",        i = "Chiseled Quartz Block", size = 16, maxInproc = 16},
    {o = "Redstone Transmission Coil", i = "HV Wire Coil",          size = 16, maxInproc = 16}
  })

  -- rubber
  slottedTemplate(plan, preActions, "rubber", "dee", sides.east, sides.south, {1}, nil, {
    {o = "Tiny Dry Rubber", i = "Birch Wood", size = 64, maxInproc = 16}
  })

  -- cinnamon
  slottedTemplate(plan, preActions, "cinnamon", "dee", sides.west, sides.south, {1}, nil, {
    {o = "Cinnamon", i = "Bone Meal", size = 64, maxInproc = 16}
  })

  -- icCompressor
  slottedTemplate(plan, preActions, "icCompressor", "540", sides.south, sides.north, {7}, function(x) return x.slot == 2 end, {
    {o = "Advanced Alloy", i = "Mixed Metal Ingot", size = 16, maxInproc = 16},
    {o = "Carbon Plate",   i = "Raw Carbon Mesh",   size = 16, maxInproc = 16}
  })

  -- icExtruding
  slottedTemplate(plan, preActions, "icExtruding", "540", sides.west, sides.north, {7}, function(x) return x.slot == 2 end, {
    {o = "Gold Cable", i = "Gold Ingot", size = 16, maxInproc = 16},
    {o = "Copper Cable", i = "Copper Ingot", size = 16, maxInproc = 16},
    {o = "Tin Cable", i = "Tin Ingot", size = 16, maxInproc = 16}
  })

  -- icRolling
  slottedTemplate(plan, preActions, "icRolling", "540", sides.down, sides.north, {7}, function(x) return x.slot == 2 end, {
    {o = "Lead Item Casing", i = "Lead Plate", size = 16, maxInproc = 16},
    {o = "Iron Item Casing", i = "Iron Plate", size = 16, maxInproc = 16},
    {o = "Tin Item Casing", i = "Tin Plate", size = 16, maxInproc = 16}
  })

  -- impregnatedStick
  slottedTemplate(plan, preActions, "impregnatedStick", "e94", sides.east, sides.south, {1}, nil, {
    {i = "Seeds", size = 1, maxInproc = 16}
  })
  slottedTemplate(plan, preActions, "impregnatedStick", "e94", sides.north, sides.south, {20}, nil, {
    {o = "Impregnated Stick", is = {{item = "Birch Wood", size = 2}}, size = 64, maxInproc = 16}
  })

  -- inscriberSilicon
  slottedTemplate(plan, preActions, "inscriberSilicon", "e94", sides.west, sides.south, {2}, nil, {
    {o = "Printed Silicon", i = "Silicon", size = 64, maxInproc = 16}
  })

  -- inscriberEngineering
  slottedTemplate(plan, preActions, "inscriberEngineering", "e94", sides.down, sides.south, {2}, nil, {
    {o = "Printed Engineering Circuit", i = "Diamond", size = 16, maxInproc = 16}
  })

  -- inscriberLogic
  slottedTemplate(plan, preActions, "inscriberLogic", "04d", sides.down, sides.north, {2}, nil, {
    {o = "Printed Logic Circuit", i = "Gold Ingot", size = 16, maxInproc = 16}
  })

  -- inscriberCalculation
  slottedTemplate(plan, preActions, "inscriberCalculation", "04d", sides.east, sides.north, {2}, nil, {
    {o = "Printed Calculation Circuit", i = "Pure Certus Quartz Crystal", size = 16, maxInproc = 16}
  })

  -- inscriberProcessor
  slottedTemplate(plan, preActions, "inscriberProcessor", "04d", sides.west, sides.north, {1, 2, 3}, nil, {
    {o = "Engineering Processor", size = 16, maxInproc = 16, is = {
      {item = "Printed Engineering Circuit", size = 1},
      {item = "Redstone", size = 1},
      {item = "Printed Silicon", size = 1}
    }},
    {o = "Logic Processor", size = 16, maxInproc = 16, is = {
      {item = "Printed Logic Circuit", size = 1},
      {item = "Redstone", size = 1},
      {item = "Printed Silicon", size = 1}
    }},
    {o = "Calculation Processor", size = 16, maxInproc = 16, is = {
      {item = "Printed Calculation Circuit", size = 1},
      {item = "Redstone", size = 1},
      {item = "Printed Silicon", size = 1}
    }}
  })

  -- crystalGrowthChamber
  heterogeneousWorkingSetTemplate(plan, preActions, "crystalGrowthChamber", "04d", sides.south, sides.north, nil, 576, nil, {
    {o = "Pure Certus Quartz Crystal", i = "Certus Quartz Seed", size = 64},
    {o = "Pure Nether Quartz Crystal", i = "Nether Quartz Seed", size = 64}
  })

  -- endStone
  slottedTemplate(plan, preActions, "endStone", "2eb", sides.south, sides.north, {1}, nil, {
    {o = "End Stone", i = "Glowstone Dust", size = 256, maxInproc = 16}
  })

  -- crafterC
  heterogeneousWorkingSetTemplate(plan, preActions, "crafterC", "2eb", sides.down, sides.north, nil, 33, nil, {
    {o = "Steel Sheetmetal", size = 16, is = {
      {item = "Steel Plate", size = 4}
    }},
    {o = "Steel Scaffolding", size = 16, is = {
      {item = "Steel Ingot", size = 3},
      {item = "Steel Rod", size = 3}
    }},
    {o = "Heavy Engineering Block", size = 16, is = {
      {item = "Uranium Plate", size = 4},
      {item = "Iron Mechanical Component", size = 2},
      {item = "Reinforced Alloy", size = 2},
      {item = "Steel Scaffolding", size = 1}
    }},
    {o = "Sturdy Casing", size = 16, is = {
      {item = "Bronze Gear", size = 2},
      {item = "Copper Gear", size = 2},
      {item = "Bronze Ingot", size = 4}
    }},
    {o = "Iron Casing", size = 16, is = {
      {item = "Iron Sheetmetal", size = 4},
      {item = "Tin Electron Tube", size = 4},
      {item = "Hardened Casing", size = 1}
    }},
    {o = "Machine Frame;rftools:machine_frame", size = 16, is = {
      {item = "Dry Rubber", size = 2},
      {item = "Machine Case", size = 1},
      {item = "Gold Gear", size = 1},
      {item = "Pink Slime", size = 2},
      {item = "Cobblestone", size = 6},
      {item = "Glass Pane", size = 1},
      {item = "Plastic", size = 2},
      {item = "Iron Bars", size = 8},
      {item = "Iron Plate", size = 8},
      {item = "Electric Motor", size = 2}
    }}
  })

  -- mekInfuserRedstone
  slottedTemplate(plan, preActions, "mekInfuserRedstone", "2eb", sides.east, sides.north, {1}, nil, {
    {o = "Basic Control Circuit", i = "Osmium Ingot", size = 64, maxInproc = 16},
    {o = "Enriched Alloy", i = "Iron Ingot", size = 64, maxInproc = 16},
    {o = "Redstone Reception Coil", i = "Gold Ingot", size = 16, maxInproc = 16},
    {o = "Redstone Conductance Coil", i = "Electrum Ingot", size = 16, maxInproc = 16},
    {o = "Energy Cell Frame", i = "Machine Frame;thermalexpansion:frame", size = 4, maxInproc = 4}
  })

  -- blackDrawer
  workingSetTemplate(plan, preActions, "2eb", sides.west, sides.north, nil, {
    {name = "thermionicFabricator", size = 1, maxInproc = 64, i = "Sand"},
    {name = "essenceOfKnowledge",   size = 1, maxInproc = 64, i = "Experience Essence"},
    {name = "gelidCryotheum",       size = 1, maxInproc = 64, i = "Cryotheum Dust"},
    {name = "resonantEnder",        size = 1, maxInproc = 64, i = "Ender Pearl"},
    {name = "energizedGlowstone",   size = 1, maxInproc = 64, i = "Glowstone Dust"},
    {name = "destabilizedRedstone", size = 1, maxInproc = 64, i = "Redstone"},
    {name = "mekInfuserRedstone",   size = 1, maxInproc = 64, i = "Compressed Redstone"},
    {name = "chlorine",             size = 1, maxInproc = 64, i = "Salt"},
    {name = "blueSlimeBlock", o = "Congealed Blue Slime Block", size = 4, maxInproc = 4,  i = "Slime Block"}
  })

  -- hardenedCasing
  slottedTemplate(plan, preActions, "hardenedCasing", "7e7", sides.east, sides.south, {20, 21}, nil, {
    {o = "Hardened Casing", size = 16, maxInproc = 16, is = {
      {item = "Sturdy Casing", size = 1},
      {item = "Diamond", size = 4}
    }}
  })

  -- tinElectronTube
  slottedTemplate(plan, preActions, "tinElectronTube", "7e7", sides.west, sides.south, {20, 21}, nil, {
    {o = "Tin Electron Tube", size = 16, maxInproc = 15, is = {
      {item = "Tin Ingot", size = 5},
      {item = "Redstone", size = 2}
    }}
  })

  -- crafterD
  heterogeneousWorkingSetTemplate(plan, preActions, "crafterD", "7e7", sides.north, sides.south, nil, 26, nil, {
    {o = "Glass Pane", size = 64, is = {
      {item = "Glass", size = 6}
    }},
    {o = "Stone Bricks", size = 64, is = {
      {item = "Stone", size = 4}
    }},
    {o = "Polished Stone", size = 64, is = {
      {item = "Stone Bricks", size = 4}
    }},
    {o = "Rich Phyto-Gro", size = 16, is = {
      {item = "Pulverized Charcoal", size = 1},
      {item = "Niter", size = 1},
      {item = "Rich Slag", size = 1}
    }},
    {o = "Raw Tofeeg", size = 16, maxInproc = 16, is = {
      {item = "Firm Tofu", size = 1},
      {item = "Dandelion Yellow", size = 1}
    }},
    {o = "Raw Tofeak", size = 16, maxInproc = 16, is = {
      {item = "Firm Tofu", size = 1},
      {item = "Soy Sauce", size = 1},
      {item = "Black Pepper", size = 1},
      {item = "Cooking Oil", size = 1},
      {item = "Mushroom;minecraft:red_mushroom", size = 1}
    }},
    {o = "Toast", size = 16, maxInproc = 16, is = {
      {item = "Butter", size = 1},
      {item = "Bread", size = 1}
    }},
    {o = "Butter", size = 16, maxInproc = 16, is = {
      {item = "Salt", size = 1},
      {item = "Silken Tofu", size = 1}
    }}
  })

  -- teMachineFrame
  slottedTemplate(plan, preActions, "teMachineFrame", "7e7", sides.down, sides.south, {20, 19, 18, 17, 16, 15}, nil, {
    {o = "Machine Frame;thermalexpansion:frame", size = 16, maxInproc = 16, is = {
      {item = "Enori Crystal", size = 4},
      {item = "Device Frame", size = 1},
      {item = "Heavy Engineering Block", size = 1},
      {item = "Iron Casing", size = 1},
      {item = "Machine Case", size = 1},
      {item = "Machine Frame;rftools:machine_frame", size = 1}
    }}
  })

  -- teTransposerXp
  slottedTemplate(plan, preActions, "teTransposerXp", "951", sides.north, sides.south, {1}, nil, {
    {o = "Blizz Powder", size = 16, maxInproc = 16, is = {{item = "Snowball", size = 2}}},
    {o = "Blaze Powder", size = 16, maxInproc = 16, is = {{item = "Sulfur", size = 2}}}
  })

  -- crafterF
  heterogeneousWorkingSetTemplate(plan, preActions, "crafterF", "951", sides.east, sides.south, nil, 24, nil, {
    {o = "Cryotheum Dust", size = 64, is = {
      {item = "Blizz Powder", size = 2},
      {item = "Redstone", size = 1},
      {item = "Snowball", size = 1}
    }},
    {o = "Pyrotheum Dust", size = 64, is = {
      {item = "Blaze Powder", size = 2},
      {item = "Redstone", size = 1},
      {item = "Sulfur", size = 1}
    }},
    {o = "Raw Toficken", size = 16, is = {
      {item = "Firm Tofu", size = 1},
      {item = "Flour;harvestcraft:flouritem", size = 1},
      {item = "Cooking Oil", size = 1},
      {item = "Spice Leaf", size = 1}
    }},
    {o = "Dough", size = 16, is = {
      {item = "Salt", size = 1},
      {item = "Flour;harvestcraft:flouritem", size = 1},
      {item = "Fresh Water", size = 1}
    }},
    {o = "Deluxe Chicken Curry", size = 256, is = {
      {item = "Chicken Curry", size = 1},
      {item = "Rice", size = 1},
      {item = "Naan", size = 1},
      {item = "Mango Chutney", size = 1}
    }},
    {o = "Black Concrete Powder", size = 16, is = {
      {item = "Ink Sac", size = 1},
      {item = "Sand", size = 4},
      {item = "Gravel", size = 4}
    }},
    {o = "HV Wire Coil", size = 16, is = {
      {item = "Aluminium Wire", size = 2},
      {item = "Steel Wire", size = 2},
      {item = "Stick", size = 1}
    }},
    {o = "Basic Capacitor", size = 16, is = {
      {item = "Redstone Transmission Coil", size = 4},
      {item = "HV Capacitor", size = 1},
      {item = "Grains of Infinity", size = 2}
    }}
  })

  -- redstoneInfuser
  slottedTemplate(plan, preActions, "redstoneInfuser", "951", sides.west, sides.south, {1}, nil, {
    {o = "Signalum Ingot",        size = 64, maxInproc = 16, i = "Shibuichi Alloy"},
    {o = "Fluxed Electrum Ingot", size = 64, maxInproc = 16, i = "Electrum Ingot" }
  })

  -- enderium
  slottedTemplate(plan, preActions, "enderium", "951", sides.down, sides.south, {1}, nil, {
    {o = "Enderium Ingot", size = 64, maxInproc = 16, i = "Lead Platinum Alloy"}
  })

  -- lumium
  slottedTemplate(plan, preActions, "lumium", "28a", sides.east, sides.north, {1}, nil, {
    {o = "Lumium Ingot", size = 64, maxInproc = 16, i = "Tin Silver Alloy"}
  })

  -- factorizer
  slottedTemplate(plan, preActions, "factorizer", "28a", sides.west, sides.north, {1}, nil, {
    {o = "Iron Nugget",                 size = 64, maxInproc = 7, i = "Iron Ingot"                       },
    {o = "Empowered Void Crystal",      size = 16, maxInproc = 2, i = "Empowered Void Crystal Block"     },
    {o = "Empowered Enori Crystal",     size = 16, maxInproc = 2, i = "Empowered Enori Crystal Block"    },
    {o = "Empowered Palis Crystal",     size = 16, maxInproc = 2, i = "Empowered Palis Crystal Block"    },
    {o = "Empowered Restonia Crystal",  size = 16, maxInproc = 2, i = "Empowered Restonia Crystal Block" },
    {o = "Empowered Emeradic Crystal",  size = 16, maxInproc = 2, i = "Empowered Emeradic Crystal Block" },
    {o = "Empowered Diamatine Crystal", size = 16, maxInproc = 2, i = "Empowered Diamatine Crystal Block"},
    {o = "Pulsating Iron Nugget",       size = 16, maxInproc = 5, i = "Pulsating Iron Ingot"             }
  })

  -- rockCrusher
  slottedTemplate(plan, preActions, "factorizer", "28a", sides.south, sides.north, {1}, nil, {
    {o = "Crushed Fluorite", size = 16, maxInproc = 16, i = "Diorite" },
    {o = "Zirconium Dust",   size = 16, maxInproc = 16, i = "Diorite" },
    {o = "Beryllium Dust",   size = 16, maxInproc = 16, i = "Andesite"}
  })

  -- empowerer
  slottedTemplate(plan, preActions, "empowerer", "28a", sides.down, sides.north, {1, 2, 3, 4, 5}, nil, {
    {o = "Empowered Enori Crystal Block", size = 16, maxInproc = 1, is = {
      {item = "Enori Crystal Block", size = 1},
      {item = "Bone Block", size = 1},
      {item = "Block of Quartz", size = 1},
      {item = "Osmium Ingot", size = 1},
      {item = "Fluorite", size = 1}
    }},
    {o = "Hardened Cell Frame", size = 4, maxInproc = 1, is = {
      {item = "Energy Cell Frame", size = 1},
      {item = "Invar Plate", size = 1},
      {item = "Invar Gear", size = 1},
      {item = "Steel Rod", size = 1},
      {item = "Steel Casing", size = 1}
    }},
    {o = "Empowered Palis Crystal Block", size = 16, maxInproc = 1, is = {
      {item = "Palis Crystal Block", size = 1},
      {item = "Congealed Blue Slime Block", size = 1},
      {item = "Sapphire", size = 1},
      {item = "Dense Lapis Lazuli Plate", size = 1},
      {item = "Cobalt Ingot", size = 1}
    }},
    {o = "Empowered Restonia Crystal Block", size = 16, maxInproc = 1, is = {
      {item = "Restonia Crystal Block", size = 1},
      {item = "Ardite Tool Rod", size = 1},
      {item = "Rhodochrosite", size = 1},
      {item = "Red Nether Brick", size = 1},
      {item = "Redstone Reception Coil", size = 1}
    }},
    {o = "Empowered Diamatine Crystal Block", size = 16, maxInproc = 1, is = {
      {item = "Diamatine Crystal Block", size = 1},
      {item = "Malachite", size = 1},
      {item = "Manyullyn Ingot", size = 1},
      {item = "Mana Diamond", size = 1},
      {item = "Zirconium Dust", size = 1}
    }},
    {o = "Empowered Void Crystal Block", size = 16, maxInproc = 1, is = {
      {item = "Void Crystal Block", size = 1},
      {item = "Basalt", size = 1},
      {item = "Block of Black Quartz", size = 1},
      {item = "Block of Black Iron", size = 1},
      {item = "Ink Sac", size = 1}
    }},
    {o = "Empowered Emeradic Crystal Block", size = 16, maxInproc = 1, is = {
      {item = "Emeradic Crystal Block", size = 1},
      {item = "Cactus Green", size = 1},
      {item = "Ethetic Green Block", size = 1},
      {item = "Beryllium Dust", size = 1},
      {item = "Emerald", size = 1}
    }}
  })

  -- alloySmelter
  slottedTemplate(plan, preActions, "alloySmelter", "28d", sides.south, sides.west, {1, 2, 3}, nil, {
    {o = "Organic Black Dye", size = 16, maxInproc = 24, is = {
      {item = "Pulverized Charcoal", size = 6},
      {item = "Slimeball", size = 1}
    }},
    {o = "Organic Green Dye", size = 16, maxInproc = 16, is = {
      {item = "Cactus Green", size = 2},
      {item = "Slimeball", size = 1},
      {item = "Pulverized Charcoal", size = 2}
    }}
  })

  -- isManaLow
  local isManaLow = true
  table.insert(preActions, {p = {op = "call", inv = "redstone", fn = "getInput", args = {sides.up}}, f = function(p)
    isManaLow = p[1] > 0
    if isManaLow then print("Mana low!", 0x00ff00) end
  end})

  -- elvenTrade
  heterogeneousWorkingSetTemplate(plan, preActions, "elvenTrade", "28d", sides.east, sides.west, nil, 9, nil, {
    {o = "Elementium Ingot", size = 64, is = {{item = "Manasteel Ingot", size = 2}}},
    {o = "Pixie Dust",       size = 64, i = "Mana Pearl"},
    {o = "Dragonstone",      size = 16, i = "Mana Diamond"}
  }, function() return isManaLow end)

  -- terrasteel
  slottedTemplate(plan, preActions, "terrasteel", "28d", sides.down, sides.west, {1, 2, 3}, nil, {
    {o = "Terrasteel Ingot", size = 16, maxInproc = 1, is = {
      {item = "Manasteel Ingot", size = 1},
      {item = "Mana Diamond", size = 1},
      {item = "Mana Pearl", size = 1}
    }}
  }, function() return isManaLow end)
end)

local cycleLime = proxyCycleTemplate("lime", function(plan, preActions, postActions)
  -- enrichment
  slottedTemplate(plan, preActions, "enrichment", "485", sides.west, sides.south, {1}, nil, {
    {o = "Compressed Redstone", size = 16, maxInproc = 16, i = "Redstone"             },
    {o = "Compressed Diamond",  size = 16, maxInproc = 16, i = "Diamond"              },
    {o = "Compressed Obsidian", size = 16, maxInproc = 16, i = "Refined Obsidian Dust"}
  })

  -- crafterG
  heterogeneousWorkingSetTemplate(plan, preActions, "crafterG", "485", sides.north, sides.south, nil, 64, nil, {
    {o = "Fiery Ingot", size = 64, is = {
      {item = "Fiery Ingot Essence", size = 8}
    }},
    {o = "Emerald", size = 256, is = {
      {item = "Emerald Essence", size = 9}
    }},
    {o = "Redstone", size = 4096, is = {
      {item = "Redstone Essence", size = 9}
    }},
    {o = "Salt", size = 256, is = {
      {item = "Fresh Water", size = 1}
    }},
    {o = "Industrial Dye Blend", size = 16, is = {
      {item = "Organic Green Dye", size = 2},
      {item = "Lapis Lazuli Dust", size = 2},
      {item = "Nether Quartz Dust", size = 4},
      {item = "Organic Black Dye", size = 1}
    }},
    {o = "Dark Iron Bars", size = 16, is = {
      {item = "Dark Steel Ingot", size = 6}
    }}
  })

  -- advancedMetallurgicFabricator
  slottedTemplate(plan, preActions, "advancedMetallurgicFabricator", "485", sides.east, sides.south, {1, 2, 3, 4, 5, 6}, nil, {
    {o = "Titanium Ingot", size = 64, maxInproc = 16, is = {
      {item = "Magnesium Ore", size = 2},
      {item = "Salt", size = 4},
      {item = "Carbon Plate", size = 1}
    }},
    {o = "Mana Infused Ingot", size = 16, maxInproc = 16, is = {
      {item = "Manasteel Ingot", size = 4},
      {item = "Diamond", size = 1}
    }}
  })

  -- limeDrawer
  workingSetTemplate(plan, preActions, "485", sides.down, sides.south, nil, {
    {name = "mekInfuserObsidian", size = 1, maxInproc = 64, i = "Compressed Obsidian"},
    {name = "moltenLumium",       size = 1, maxInproc = 64, i = "Lumium Ingot"       },
    {name = "extremeReactor",     size = 1, maxInproc = 64, i = "Uranium Ingot"      }
  })

  -- mekInfuserObsidian
  slottedTemplate(plan, preActions, "mekInfuserObsidian", "99a", sides.down, sides.north, {1}, nil, {
    {o = "Atomic Alloy", size = 16, maxInproc = 16, i = "Reinforced Alloy"}
  })

  -- teTransposerRedstone
  slottedTemplate(plan, preActions, "teTransposerRedstone", "99a", sides.west, sides.north, {1}, nil, {
    {o = "Flux Crystal", size = 16, maxInproc = 16, i = "Diamond"}
  })

  -- teTransposerLumium
  slottedTemplate(plan, preActions, "teTransposerLumium", "99a", sides.south, sides.north, {1}, nil, {
    {o = "Reinforced Cell Frame (Full)", size = 4, maxInproc = 4, i = "Reinforced Cell Frame (Empty)"}
  })

  -- metalPressPlate
  heterogeneousWorkingSetTemplate(plan, preActions, "metalPressPlate", "99a", sides.east, sides.north, nil, 27, nil, {
    {o = "Electrum Large Plate", size = 4, i = "Block of Electrum"}
  })

  -- extremeReactor
  table.insert(preActions, {p = {op = "call", inv = "br_reactor", fn = "getEnergyStored", args = {}}, f = function(p)
    local on = p[1] < 5000000 or getAvail("Cyanite Ingot", true) < 256
    table.insert(postActions, {p = {op = "call", inv = "br_reactor", fn = "setActive", args = {on}}})
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
    cpsAll(cycleCenter, cycleBlack, cycleLime),
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
