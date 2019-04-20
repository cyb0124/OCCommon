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

local TIMEOUT = 1
local NAGLING = 0.2
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
    if n > 0 then
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
      if #buf == 0 then
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

local serpent = (function()
  local n, v = "serpent", "0.301" -- (C) 2012-17 Paul Kulchenko; MIT License
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
            out[#out+1] = val2str(value,key,indent,insref,seen[t],plainindex,level+1)
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

local HEARTBEAT_INTERVAL = 2
local HEARTBEAT_TIMEOUT = 5

-- Assuming: lpsDump, LpsParser, Rx, Tx
-- callbacks: push, connected, error, packet

local Peer = function(dispatch, callbacks, passive)
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
  local StateConnect, StateRun
  StateConnect = function()
    local state = {}
    local alive = true
    local timer
    state.push = function(data)
      changeState(StateRun())
    end
    state.close = function()
      if alive then
        if timer then timer() end
        alive = false
      end
    end
    dispatch.queue(function()
      if alive then
        callbacks.push{seq = 0, dat = "", ack = 0, wnd = BUFFER}
        timer = dispatch.setTimer(HEARTBEAT_TIMEOUT, function()
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
        rx.push(data)
        tx.push(data)
      end
    end
    lpsParser = LpsParser(function(packet)
      if packet ~= "heartbeat" then
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
          Peer.send("heartbeat")
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
          callbacks.push{seq = 0, dat = "", ack = 0, wnd = BUFFER}
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

local sides = {
  bottom = 0, down  = 0, yn = 0,
  top    = 1, up    = 1, yp = 1,
  back   = 2, north = 2, zn = 2,
  front  = 3, south = 3, zp = 3,
  right  = 4, west  = 4, xn = 4,
  left   = 5, east  = 5, xp = 5
}

local modem = component.proxy(component.list("modem")())
local inv = component.proxy(component.list("transposer")())
local side = sides.top

modem.open(1)
local nSlots = inv.getInventorySize(side)
local clients = {}
local allocations = {}
local waitings = {}

local onSlotFree = function(slot)
  if inv.getStackInSlot(side, slot) then
    computer.beep(880)
  else
    for waiting, _ in pairs(waitings) do
      waiting(slot)
      break
    end
  end
end

local Client = function(addr)
  local Client = {}
  local myAllocations = {}
  local myWaitings = {}
  local peer
  peer = Peer(dispatch, {
    push = function(packet)
      packet.from = "allocator"
      packet.to = addr
      modem.broadcast(1, serpent.dump(packet))
    end,
    connected = function() end
    error = function()
      Client.close()
    end,
    packet = function(packet)
      if packet.op == "allocate" then
        local done
        for i = 1, #nSlots do
          if not allocations[i] and not inv.getStackInSlot(side, i) then
            done = true
            allocations[i] = true
            myAllocations[i] = true
            peer.send(i)
            break
          end
        end
        if not done then
          local newWaiting
          newWaiting = function(slot)
            waitings[newWaiting] = nil
            myWaitings[newWaiting] = nil
            myAllocations[slot] = true
            allocations[slot] = true
            peer.send(i)
          end
        end
      elseif packet.op == "free" then
        myAllocations[packet.slot] = nil
        allocations[packet.slot] = nil
        onSlotFree(packet.slot)
      elseif packet.op == "close" then
        Client.close()
      end
    end
  }, true)
  Client.push = peer.push
  Client.close = function()
    clients[addr] = nil
    peer.close()
    for waiting, _ in pairs(myWaitings) do
      waitings[waiting] = nil
    end
    for slot, _ in pairs(myAllocations) do
      allocations[slot] = nil
      onSlotFree(slot)
    end
  end
  return Client
end

local dispatch = Dispatch(function(name, _, _, _, _, raw)
  if name == "modem_message" then
    local packet = load(raw)()
    if packet.to == "allocator" then
      if clients[packet.from] then
        clients[packet.from].push(packet)
      else
        clients[packet.from] = Client(packet.from)
      end
    end
  end
end)

(function()
  local timer
  local startTimer = function()
    local new
    new = dispatch.setTimer(3, function()
      if timer == new then
        startTimer()
        for i = 1, #nSlots do
          if not allocations[i] then
            onSlotFree(i)
          end
        end
      end
    end)
    timer = new
  end
end)()
