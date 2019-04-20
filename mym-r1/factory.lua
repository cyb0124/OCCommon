local round = function(num)
  return math.floor(num + 0.5)
end

local toKB = function(num)
  return tostring(round(num / 1024))
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

local json = (function()
  --
  -- json.lua
  --
  -- Copyright (c) 2018 rxi
  --
  -- Permission is hereby granted, free of charge, to any person obtaining a copy of
  -- this software and associated documentation files (the "Software"), to deal in
  -- the Software without restriction, including without limitation the rights to
  -- use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
  -- of the Software, and to permit persons to whom the Software is furnished to do
  -- so, subject to the following conditions:
  --
  -- The above copyright notice and this permission notice shall be included in all
  -- copies or substantial portions of the Software.
  --
  -- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  -- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  -- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  -- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  -- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  -- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  -- SOFTWARE.
  --

  local json = { _version = "0.1.0" }

  -------------------------------------------------------------------------------
  -- Encode
  -------------------------------------------------------------------------------

  local encode

  local escape_char_map = {
    [ "\\" ] = "\\\\",
    [ "\"" ] = "\\\"",
    [ "\b" ] = "\\b",
    [ "\f" ] = "\\f",
    [ "\n" ] = "\\n",
    [ "\r" ] = "\\r",
    [ "\t" ] = "\\t",
  }

  local escape_char_map_inv = { [ "\\/" ] = "/" }
  for k, v in pairs(escape_char_map) do
    escape_char_map_inv[v] = k
  end


  local function escape_char(c)
    return escape_char_map[c] or string.format("\\u%04x", c:byte())
  end


  local function encode_nil(val)
    return "null"
  end


  local function encode_table(val, stack)
    local res = {}
    stack = stack or {}

    -- Circular reference?
    if stack[val] then error("circular reference") end

    stack[val] = true

    if val[1] ~= nil or next(val) == nil then
      -- Treat as array -- check keys are valid and it is not sparse
      local n = 0
      for k in pairs(val) do
        if type(k) ~= "number" then
          error("invalid table: mixed or invalid key types")
        end
        n = n + 1
      end
      if n ~= #val then
        error("invalid table: sparse array")
      end
      -- Encode
      for i, v in ipairs(val) do
        table.insert(res, encode(v, stack))
      end
      stack[val] = nil
      return "[" .. table.concat(res, ",") .. "]"

    else
      -- Treat as an object
      for k, v in pairs(val) do
        if type(k) ~= "string" then
          error("invalid table: mixed or invalid key types")
        end
        table.insert(res, encode(k, stack) .. ":" .. encode(v, stack))
      end
      stack[val] = nil
      return "{" .. table.concat(res, ",") .. "}"
    end
  end


  local function encode_string(val)
    return '"' .. val:gsub('[%z\1-\31\\"]', escape_char) .. '"'
  end


  local function encode_number(val)
    -- Check for NaN, -inf and inf
    if val ~= val or val <= -math.huge or val >= math.huge then
      error("unexpected number value '" .. tostring(val) .. "'")
    end
    return string.format("%.14g", val)
  end


  local type_func_map = {
    [ "nil"     ] = encode_nil,
    [ "table"   ] = encode_table,
    [ "string"  ] = encode_string,
    [ "number"  ] = encode_number,
    [ "boolean" ] = tostring,
  }


  encode = function(val, stack)
    local t = type(val)
    local f = type_func_map[t]
    if f then
      return f(val, stack)
    end
    error("unexpected type '" .. t .. "'")
  end


  function json.encode(val)
    return ( encode(val) )
  end


  -------------------------------------------------------------------------------
  -- Decode
  -------------------------------------------------------------------------------

  local parse

  local function create_set(...)
    local res = {}
    for i = 1, select("#", ...) do
      res[ select(i, ...) ] = true
    end
    return res
  end

  local space_chars   = create_set(" ", "\t", "\r", "\n")
  local delim_chars   = create_set(" ", "\t", "\r", "\n", "]", "}", ",")
  local escape_chars  = create_set("\\", "/", '"', "b", "f", "n", "r", "t", "u")
  local literals      = create_set("true", "false", "null")

  local literal_map = {
    [ "true"  ] = true,
    [ "false" ] = false,
    [ "null"  ] = nil,
  }


  local function next_char(str, idx, set, negate)
    for i = idx, #str do
      if set[str:sub(i, i)] ~= negate then
        return i
      end
    end
    return #str + 1
  end


  local function decode_error(str, idx, msg)
    local line_count = 1
    local col_count = 1
    for i = 1, idx - 1 do
      col_count = col_count + 1
      if str:sub(i, i) == "\n" then
        line_count = line_count + 1
        col_count = 1
      end
    end
    error( string.format("%s at line %d col %d", msg, line_count, col_count) )
  end


  local function codepoint_to_utf8(n)
    -- http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=iws-appendixa
    local f = math.floor
    if n <= 0x7f then
      return string.char(n)
    elseif n <= 0x7ff then
      return string.char(f(n / 64) + 192, n % 64 + 128)
    elseif n <= 0xffff then
      return string.char(f(n / 4096) + 224, f(n % 4096 / 64) + 128, n % 64 + 128)
    elseif n <= 0x10ffff then
      return string.char(f(n / 262144) + 240, f(n % 262144 / 4096) + 128,
                         f(n % 4096 / 64) + 128, n % 64 + 128)
    end
    error( string.format("invalid unicode codepoint '%x'", n) )
  end


  local function parse_unicode_escape(s)
    local n1 = tonumber( s:sub(3, 6),  16 )
    local n2 = tonumber( s:sub(9, 12), 16 )
    -- Surrogate pair?
    if n2 then
      return codepoint_to_utf8((n1 - 0xd800) * 0x400 + (n2 - 0xdc00) + 0x10000)
    else
      return codepoint_to_utf8(n1)
    end
  end


  local function parse_string(str, i)
    local has_unicode_escape = false
    local has_surrogate_escape = false
    local has_escape = false
    local last
    for j = i + 1, #str do
      local x = str:byte(j)

      if x < 32 then
        decode_error(str, j, "control character in string")
      end

      if last == 92 then -- "\\" (escape char)
        if x == 117 then -- "u" (unicode escape sequence)
          local hex = str:sub(j + 1, j + 5)
          if not hex:find("%x%x%x%x") then
            decode_error(str, j, "invalid unicode escape in string")
          end
          if hex:find("^[dD][89aAbB]") then
            has_surrogate_escape = true
          else
            has_unicode_escape = true
          end
        else
          local c = string.char(x)
          if not escape_chars[c] then
            decode_error(str, j, "invalid escape char '" .. c .. "' in string")
          end
          has_escape = true
        end
        last = nil

      elseif x == 34 then -- '"' (end of string)
        local s = str:sub(i + 1, j - 1)
        if has_surrogate_escape then
          s = s:gsub("\\u[dD][89aAbB]..\\u....", parse_unicode_escape)
        end
        if has_unicode_escape then
          s = s:gsub("\\u....", parse_unicode_escape)
        end
        if has_escape then
          s = s:gsub("\\.", escape_char_map_inv)
        end
        return s, j + 1

      else
        last = x
      end
    end
    decode_error(str, i, "expected closing quote for string")
  end


  local function parse_number(str, i)
    local x = next_char(str, i, delim_chars)
    local s = str:sub(i, x - 1)
    local n = tonumber(s)
    if not n then
      decode_error(str, i, "invalid number '" .. s .. "'")
    end
    return n, x
  end


  local function parse_literal(str, i)
    local x = next_char(str, i, delim_chars)
    local word = str:sub(i, x - 1)
    if not literals[word] then
      decode_error(str, i, "invalid literal '" .. word .. "'")
    end
    return literal_map[word], x
  end


  local function parse_array(str, i)
    local res = {}
    local n = 1
    i = i + 1
    while 1 do
      local x
      i = next_char(str, i, space_chars, true)
      -- Empty / end of array?
      if str:sub(i, i) == "]" then
        i = i + 1
        break
      end
      -- Read token
      x, i = parse(str, i)
      res[n] = x
      n = n + 1
      -- Next token
      i = next_char(str, i, space_chars, true)
      local chr = str:sub(i, i)
      i = i + 1
      if chr == "]" then break end
      if chr ~= "," then decode_error(str, i, "expected ']' or ','") end
    end
    return res, i
  end


  local function parse_object(str, i)
    local res = {}
    i = i + 1
    while 1 do
      local key, val
      i = next_char(str, i, space_chars, true)
      -- Empty / end of object?
      if str:sub(i, i) == "}" then
        i = i + 1
        break
      end
      -- Read key
      if str:sub(i, i) ~= '"' then
        decode_error(str, i, "expected string for key")
      end
      key, i = parse(str, i)
      -- Read ':' delimiter
      i = next_char(str, i, space_chars, true)
      if str:sub(i, i) ~= ":" then
        decode_error(str, i, "expected ':' after key")
      end
      i = next_char(str, i + 1, space_chars, true)
      -- Read value
      val, i = parse(str, i)
      -- Set
      res[key] = val
      -- Next token
      i = next_char(str, i, space_chars, true)
      local chr = str:sub(i, i)
      i = i + 1
      if chr == "}" then break end
      if chr ~= "," then decode_error(str, i, "expected '}' or ','") end
    end
    return res, i
  end


  local char_func_map = {
    [ '"' ] = parse_string,
    [ "0" ] = parse_number,
    [ "1" ] = parse_number,
    [ "2" ] = parse_number,
    [ "3" ] = parse_number,
    [ "4" ] = parse_number,
    [ "5" ] = parse_number,
    [ "6" ] = parse_number,
    [ "7" ] = parse_number,
    [ "8" ] = parse_number,
    [ "9" ] = parse_number,
    [ "-" ] = parse_number,
    [ "t" ] = parse_literal,
    [ "f" ] = parse_literal,
    [ "n" ] = parse_literal,
    [ "[" ] = parse_array,
    [ "{" ] = parse_object,
  }


  parse = function(str, idx)
    local chr = str:sub(idx, idx)
    local f = char_func_map[chr]
    if f then
      return f(str, idx)
    end
    decode_error(str, idx, "unexpected character '" .. chr .. "'")
  end


  function json.decode(str)
    if type(str) ~= "string" then
      error("expected argument of type string, got " .. type(str))
    end
    local res, idx = parse(str, next_char(str, 1, space_chars, true))
    idx = next_char(str, idx, space_chars, true)
    if idx <= #str then
      decode_error(str, idx, "trailing garbage")
    end
    return res
  end


  return json
end)()

-- Assuming: json

local lpsDump = function(x)
  x = json.encode(x)
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
        callback(json.decode(string.sub(buffer, 1, length)))
        changeState(StateInit())
        LpsParser.push(string.sub(buffer, length + 1))
      end
    end
    return state
  end

  changeState(StateInit())

  return LpsParser
end

local TIMEOUT = 1
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

local HEARTBEAT_INTERVAL = 30
local HEARTBEAT_TIMEOUT = 60
local CONNECT_TIMEOUT = 2

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
  return function(cont, ...)
    cont(f(...))
  end
end

local cpsThen = function(dispatch, f, g)
  return function(cont)
    f(function(...)
      local xs = {...}
      dispatch.queue(function()
        g(cont, table.unpack(xs))
      end)
    end)
  end
end

local cpsChain = function(dispatch, f, ...)
  local fs = {...}
  for i = 1, #fs do
    f = cpsThen(dispatch, f, fs[i])
  end
  return f
end

local cpsAll = function(dispatch, ...)
  local fs = {...}
  return function(done, ...)
    local nFinished = 0
    local args = {...}
    local results = {}
    for i = 1, #fs do
      dispatch.queue(function()
        fs[i](function(...)
          results[i] = {...}
          nFinished = nFinished + 1
          if nFinished == #fs then
            done(results)
          end
        end, table.unpack(args))
      end)
    end
  end
end

local cpsWrap = function(dispatch, f)
  return function(cont, ...)
    local co, resume = coroutine.create(f)
    resume = function(...)
      local result = {coroutine.resume(co, ...)}
      dispatch.queue(function()
        if not result[1] then
          error(result[2])
        else
          if result[2] == "done" then
            cont(select(3, table.unpack(result)))
          elseif result[2] == "then" then
            result[3](resume, select(4, table.unpack(result)))
          else
            error "invalid yield"
          end
        end
      end)
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

local me = component.proxy(resolve("me_interface"))
local inv = component.proxy(resolve("transposer"))
local dbAddr = resolve("database")
local db = component.proxy(dbAddr)
local modem = component.proxy(resolve("modem"))
local channel = 1
local hostname = "factory"
local sideME = sides.down
local sideBus = sides.zn
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

modem.open(channel)
local iClients, oClients, IClient = {}, {}
local dispatch = Dispatch(function(kind, ...)
  if kind == "modem_message" then
    local data, remoteUUID, from, to = json.decode(select(5, ...)), (select(2, ...)), select(6, ...)
    if to == hostname then
      local client = iClients[from]
      if client then
        client.remoteUUID = remoteUUID
        client.peer.push(data)
      elseif data.seq then
        modem.send(remoteUUID, channel, json.encode{rst = true}, hostname, from)
      elseif not data.rst then
        IClient(from, remoteUUID)
      end
    elseif string.match(to, "[^:]*") == hostname then
      local client = oClients[to]
      if client and from == client.remoteAddr then
        client.remoteUUID = remoteUUID
        client.peer.push(data)
      else
        modem.send(remoteUUID, channel, json.encode{rst = true}, to, from)
      end
    end
  end
end)

local sourcePortCounter = math.floor(math.random(0x1000))
local nextSourceAddr = function()
  local result = hostname .. ":" .. sourcePortCounter
  sourcePortCounter = sourcePortCounter + 1
  if sourcePortCounter > 0x1000 then
    sourcePortCounter = 1
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
    cont = cpsWrap(dispatch, cont)
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
      modem.send(result.remoteUUID, channel, json.encode(x), hostname, from)
    end,
    connected = function()
      result.changeHandler(HandlerCPS(result, true, cpsHandleInit))
    end,
    error = function()
      result.changeHandler(nil)
      iClients[from] = nil
      print("[-]" .. from, 0x0000ff)
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
  result.localAddr = nextSourceAddr()
  result.peer = Peer(dispatch, {
    push = function(x)
      if result.remoteUUID then
        modem.send(result.remoteUUID, channel, json.encode(x), result.localAddr, result.remoteAddr)
      else
        modem.broadcast(channel, json.encode(x), result.localAddr, result.remoteAddr)
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

local busAllocations, busWaitQueue, insertJunk = {}, {}
local busUpdate = function()
  local stacks = inv.getAllStacks(sideBus)
  for i = 1, stacks.count() do
    local item = stacks()
    if not busAllocations[i] then
      if not item.name then item.size = 0 end
      if item.size > 0 then
        busAllocations[i] = true
        print("Bus: unexpected " .. item.label .. "*"
          .. math.floor(item.size) .. "@" .. i, 0xFF00000, 660)
        insertJunk(i, item, item.size)
      elseif #busWaitQueue > 0 then
        busAllocations[i] = true
        local callback = table.remove(busWaitQueue)
        dispatch.queue(function()
          callback(i)
        end)
      end
    end
  end
  if #busWaitQueue <= 0 then
    busWaitQueue = {}
  end
end
local busAllocate = function(callback)
  table.insert(busWaitQueue, callback)
  busUpdate()
end
local busAllocateN = function(callback, nRequired)
  local nFinished, slots = 0, {}
  for i = 1, nRequired do
    table.insert(busWaitQueue, function(slot)
      nFinished = nFinished + 1
      slots[i] = slot
      if nFinished == nRequired then
        callback(slots)
      end
    end)
  end
  busUpdate()
end
local busFree = function(slot)
  busAllocations[slot] = nil
  busUpdate()
end
local busFreeN = function(slots)
  for _, slot in ipairs(slots) do
    busAllocations[slot] = nil
  end
  busUpdate()
end

-- Inventory utils

local forEachSlot = function(inv, side, f)
  local stacks = inv.getAllStacks(side)
  local count = stacks.count()
  for slot = 1, count do
    local item = stacks()
    if item.name then
      item.size = math.floor(item.size)
      f(item, slot)
    end
  end
  return count
end

-- Item management

-- item: {name, label, damage, maxDamage, hasTag, enchantments, ...}
-- items: [{item, interface, avail, priority}]
-- maps: string -> [{item, interface, avail, priority}]
-- canExtract(cont) : [{item, avail, priority}], extract(cont, item, size, slot)
-- canInsert(cont, item) : priority or nil, insert(cont, item, slot, size)
local itemInterfaces, items, nameMap, labelMap, nameLabelMap = {}

local getItem = function(x)
  if type(x) == "string" then
    return nameMap[x] or labelMap[x] or nameLabelMap[x]
  else
    return x
  end
end

local getAvail = function(x)
  local avail = 0
  x = getItem(x)
  if x then
    for _, x in ipairs(x) do
      avail = avail + x.avail
    end
  end
  return avail
end

local getListAvail = function(itemList)
  local result = math.huge
  for _, v in pairs(itemList) do
    result = math.min(result, math.floor(getAvail(v.item) / v.size))
  end
  return result
end

local getListSum = function(itemList)
  local result = 0
  for _, v in pairs(itemList) do
    result = result + v.size
  end
  return result
end

local rankRecipes = function(recipes, adjust)
  for _, recipe in pairs(recipes) do
    if recipe.is then
      recipe.ia = getListAvail(recipe.is)
    else
      recipe.ia = getAvail(recipe.i)
    end
    recipe.oa = getAvail(recipe.o)
    if recipe.ia <= 0 then
      recipe.demand = -math.huge
    else
      recipe.demand = 1 - recipe.oa / recipe.size
    end
    if adjust then
      adjust(recipe)
    end
  end
  table.sort(recipes, function(x, y)
    return x.demand > y.demand
  end)
end

local extractItem = function(item, size, reason)
  item = getItem(item)
  local result = {
    size = 0,
    extract = function(cont, slot) dispatch.queue(cont) end
  }
  if item then
    local actions, remaining, label = {}, size
    while remaining > 0 and #item > 0 do
      local source = item[1]
      local toUse = math.min(remaining, source.avail)
      source.avail = source.avail - toUse
      if source.avail <= 0 then table.remove(item, 1) end
      remaining = remaining - toUse
      result.size = result.size + toUse
      table.insert(actions, function(cont, slot)
        source.interface.extract(cont, source.item, toUse, slot)
      end)
      label = source.item.label
    end
    if #actions > 0 then
      if reason then
        reason = reason .. ": "
      else
        reason = ""
      end
      print(reason .. label .. "*" .. math.floor(result.size))
      result.extract = cpsAll(dispatch, table.unpack(actions))
    end
  end
  return result
end

local insertItem = cpsWrap(dispatch, function(slot, item, size)
  item.size = nil
  local validInterfaces, actions = {}, {}
  for i, interface in ipairs(itemInterfaces) do
    if interface.canInsert then
      table.insert(validInterfaces, interface.insert)
      table.insert(actions, function(cont)
        interface.canInsert(cont, item)
      end)
    end
  end
  local priorities, bestDrain, bestPriority = coroutine.yield("then", cpsAll(dispatch, table.unpack(actions)))
  for i, interface in ipairs(validInterfaces) do
    local priority = priorities[i][1]
    if priority and (not bestDrain or priority > bestPriority) then
      bestDrain = interface
      bestPriority = priority
    end
  end
  if bestDrain then
    coroutine.yield("then", bestDrain, item, slot, size)
  else
    print("Can't find drain for " .. item.label .. "*"
      .. math.floor(size) .. "@" .. slot, 0xFF00000, 880)
  end
  return "done"
end)

insertJunk = function(slot, item, size)
  insertItem(function() busFree(slot) end, slot, item, size)
end

local selectMaxAvail = function(x, ...)
  local xs = {...}
  for i = 1, #xs do
    local now = xs[i]
    if getAvail(now) > getAvail(x) then
      x = now
    end
  end
  return x
end

-- Item Interfaces

table.insert(itemInterfaces, {
  canExtract = cps(function()
    local items, infos = me.getItemsInNetwork(), {}
    for _, item in ipairs(items) do
      table.insert(infos, {item = item, avail = item.size, priority = 0})
      item.size = nil
    end
    return infos
  end),
  canInsert = cps(function(item)
    return 0
  end),
  extract = cps(function(item, size, slot)
    db.clear(1)
    me.store(item, dbAddr, 1, 1)
    me.setInterfaceConfiguration(1, dbAddr, 1, size)
    inv.transferItem(sideME, sideBus, size, 1, slot)
    me.setInterfaceConfiguration(1)
  end),
  insert = cps(function(item, slot, size)
    inv.transferItem(sideBus, sideME, size, slot, 9)
  end)
})

local itemInterfaceTemplate1 = function(node, filter)
  local conn, mutex = wrappedConnUtils(node), WrappedMutex(dispatch)
  table.insert(itemInterfaces, {
    canExtract = cpsWrap(dispatch, function()
      mutex.lock()
      while true do
        conn.connect()
        conn.send{op = "canExtract"}
        local p = conn.recv()
        if p then
          mutex.unlock()
          return "done", p.items
        end
      end
    end),
    canInsert = cps(function(item)
      if type(filter) == "function" then
        if filter(item) then return 1 end
      else
        if filter[item.name] or filter[item.label]
        or filter[item.name .. ";" .. item.label] then return 1 end
      end
    end),
    extract = cpsWrap(dispatch, function(item, size, slot)
      mutex.lock()
      while true do
        conn.connect()
        conn.send{op = "extract", slot = slot, size = size, name = item.name, label = item.label}
        if conn.recv() then
          mutex.unlock()
          return "done"
        end
      end
    end),
    insert = cpsWrap(dispatch, function(item, slot, size)
      mutex.lock()
      while true do
        conn.connect()
        conn.send{op = "insert", slot = slot, size = size, name = item.name, label = item.label}
        if conn.recv() then
          mutex.unlock()
          return "done"
        end
      end
    end)
  })
end

itemInterfaceTemplate1("node3", {
  ["Coal"] = true, ["Gravel"] = true, ["Iron Ingot"] = true, ["Apatite"] = true,
  ["thermalfoundation:material;Copper Ingot"] = true, ["thermalfoundation:material;Tin Ingot"] = true,
  ["Black Quartz"] = true, ["Lapis Lazuli"] = true, ["Redstone"] = true,
  ["thermalfoundation:material;Lead Ingot"] = true, ["Uranium Ingot"] = true
})

itemInterfaceTemplate1("node4", function(item)
  if string.match(item.label, " Ore$") then return true
  elseif string.match(item.label, "^Pulverized ") then return true
  elseif item.label == "Draconium Dust" then return true
  elseif item.label == "Crushed Black Quartz" then return true
  elseif item.label == "Sand" then return true
  elseif item.label == "Stone" and item.name == "minecraft:stone" then return true
  elseif item.name == "minecraft:cobblestone" then return true
  elseif item.name == "draconicevolution:draconium_ore" then return true end
end)

-- Handlers

local cycleHooks = {}

local cpsHandleStore = function(conn, p)
  local slots = coroutine.yield("then", busAllocateN, p.n)
  conn.send{slots = slots}
  conn.recv()
  local actions, reverseMap = {}, {}
  for _, slot in ipairs(slots) do reverseMap[slot] = true end
  forEachSlot(inv, sideBus, function(item, slot)
    if reverseMap[slot] then
      print("From " .. conn.getFrom() .. ": " .. item.label .. "*" .. item.size)
      table.insert(actions, function(cont)
        insertItem(cont, slot, item, item.size)
      end)
    end
  end)
  coroutine.yield("then", cpsAll(dispatch, table.unpack(actions)))
  busFreeN(slots)
end

local cpsHandleTemplate1 = function(conn, p, item, reason)
  local slot = coroutine.yield("then", busAllocate)
  local cycleCont = coroutine.yield("then", function(cont) table.insert(cycleHooks, cont) end)
  coroutine.yield("then", extractItem(item, p.n, reason).extract, slot)
  dispatch.queue(cycleCont)
  conn.send{slot = slot}
  conn.recv()
  busFree(slot)
end

cpsHandleInit = function(conn)
  while true do
    local p = conn.recv()
    if not p then return "done" end
    if p.op == "store" then
      cpsHandleStore(conn, p)
    elseif p.op == "sand" then
      cpsHandleTemplate1(conn, p, "Sand", "sandInduction")
    elseif p.op == "fuel" then
      cpsHandleTemplate1(conn, p, "Uranium Ingot", "reactorFuel")
    else
      conn.close()
    end
  end
end

-- Cycle

local cycleCounter = 1

local cycleStart = cpsWrap(dispatch, function()
  print("Cycle #" .. cycleCounter, 0x7FFF00)
  items, nameMap, labelMap, nameLabelMap = {}, {}, {}, {}
  local addToMap = function(map, key, value)
    if map[key] then
      table.insert(map[key], value)
    else
      map[key] = {value}
    end
  end
  local prioritizeMap = function(map)
    for _, item in pairs(map) do
      table.sort(item, function(x, y) return x.priority > y.priority end)
    end
  end
  local actions = {}
  for i, interface in ipairs(itemInterfaces) do
    actions[i] = interface.canExtract
  end
  local infos = coroutine.yield("then", cpsAll(dispatch, table.unpack(actions)))
  for i, infos in ipairs(infos) do
    for _, info in ipairs(infos[1]) do
      if info.avail > 0 then
        info.interface = itemInterfaces[i]
        table.insert(items, info)
        addToMap(nameMap, info.item.name, info)
        addToMap(labelMap, info.item.label, info)
        addToMap(nameLabelMap, info.item.name .. ";" .. info.item.label, info)
      end
    end
  end
  prioritizeMap(nameMap)
  prioritizeMap(labelMap)
  prioritizeMap(nameLabelMap)
  print("Mem: " .. toKB(computer.freeMemory())
      .. "/" .. toKB(computer.totalMemory()))
  return "done"
end)

local cycleRunHooks = function(cont)
  local nRemain = #cycleHooks
  if nRemain <= 0 then
    cont()
  else
    local nowHooks = cycleHooks
    cycleHooks = {}
    for i = 1, nRemain do
      nowHooks[i](function()
        nRemain = nRemain - 1
        if nRemain <= 0 then
          cont()
        end
      end)
    end
  end
end

local cycleEnd = cpsWrap(dispatch, function()
  items, nameMap, labelMap, nameLabelMap = nil
  busUpdate()
  cycleCounter = cycleCounter + 1
  return "done"
end)

local cycleTemplate1 = function(host, op, recipes, batch)
  return cpsWrap(dispatch, function()
    rankRecipes(recipes, function(recipe)
      if recipe.demand > 0 and not recipe.o then
        recipe.demand = 1E-6
      end
    end)
    if recipes[1].demand > 0 then
      local conn = wrappedConnUtils(host)
      conn.connect()
      conn.send{op = op}
      local p = conn.recv()
      if p then
        local inproc, slots = p.n, {}
        for i = 1, #recipes do
          local recipe = recipes[i]
          if recipe.demand <= 0 then break end
          local toproc = math.min(batch - inproc, recipe.size - recipe.oa - inproc)
          local item = getItem(recipes[i].i)
          if item then
            toproc = extractItem(item, toproc, op)
            if toproc.size > 0 then
              inproc = inproc + toproc.size
              local slot = coroutine.yield("then", busAllocate)
              coroutine.yield("then", toproc.extract, slot)
              table.insert(slots, slot)
            end
          end
        end
        if #slots > 0 then
          conn.send{slots = slots}
          conn.recv()
          busFreeN(slots)
        end
      end
      conn.close()
    end
    return "done"
  end)
end

local cycleTemplate2 = function(host, op, input, output, toStock, unit, batch)
  return cpsWrap(dispatch, function()
    local toStock, item = 64, getItem(output)
    local avail = getAvail(input)
    if avail < toStock then
      local conn = wrappedConnUtils(host)
      conn.connect()
      local slot = coroutine.yield("then", busAllocate)
      conn.send{op = op}
      local p = conn.recv()
      if p then
        local toproc = math.floor(math.min(toStock - avail - p.n + unit - 1, batch - p.n, getAvail(input)) / unit) * unit
        if toproc > 0 then
          coroutine.yield("then", extractItem(input, toproc, op).extract, slot)
          conn.send{slot = slot}
          conn.recv()
        end
      end
      busFree(slot)
      conn.close()
    end
    return "done"
  end)
end

local cyclePulverizer = cycleTemplate1("node1", "pulverizer", {
  {i = "minecraft:cobblestone", o = "Sand", size = 4096},
  {i = "Oak Wood", o = "thermalfoundation:material;Sawdust", size = 64},
  {i = "Iron Ore", size = math.huge},
  {i = "Copper Ore", size = math.huge},
  {i = "Gold Ore", size = math.huge},
  {i = "Black Quartz Ore", size = math.huge},
  {i = "Coal Ore", size = math.huge},
  {i = "Redstone Ore", size = math.huge},
  {i = "Lapis Lazuli Ore", size = math.huge},
  {i = "Certus Quartz Ore", size = math.huge},
  {i = "Charged Certus Quartz Ore", size = math.huge},
  {i = "Nether Quartz Ore", size = math.huge},
  {i = "Emerald Ore", size = math.huge},
  {i = "Diamond Ore", size = math.huge},
  {i = "Apatite Ore", size = math.huge},
  {i = "draconicevolution:draconium_ore", size = math.huge}
}, 64)

local cycleFurnace = cycleTemplate1("node1", "furnace", {
  {i = "Sand",     o = "minecraft:glass", size = 4096},
  {i = "Oak Wood", o = "Charcoal", size = 64},
  {i = "Raw Fish", o = "Cooked Fish", size = 64},
  {i = "Crushed Black Quartz", size = math.huge},
  {i = "minecraft:cobblestone", o = "minecraft:stone;Stone", size = 256}
}, 64)

local cycleSandInduction = cycleTemplate1("node2", "sandInduction", {
  {i = "Pulverized Gold", size = math.huge},
  {i = "Pulverized Iron", size = math.huge},
  {i = "Pulverized Copper", size = math.huge},
  {i = "Pulverized Nickel", size = math.huge},
  {i = "Pulverized Silver", size = math.huge},
  {i = "Pulverized Lead", size = math.huge},
  {i = "Draconium Dust", size = math.huge},
  {i = "Yellorite Ore", size = math.huge},
  {i = "Tin Ore", size = math.huge},
  {i = "Lead Ore", size = math.huge},
  {i = "Silver Ore", size = math.huge},
  {i = "Aluminum Ore", size = math.huge},
  {i = "Uranium Ore", size = math.huge}
}, 64)

local cyclePaper = cycleTemplate2("node2", "paper", "thermalfoundation:material;Sawdust", "Paper", 64, 4, 64)

local cycleCobble = cpsWrap(dispatch, function()
  local toStock, avail = 4096, getAvail("minecraft:cobblestone")
  if avail < toStock then
    local conn = wrappedConnUtils("node1")
    print("Making Cobblestone")
    conn.connect()
    local slot = coroutine.yield("then", busAllocate)
    conn.send{op = "cobble", n = toStock - avail, slot = slot}
    conn.recv()
    local item = inv.getStackInSlot(sideBus, slot)
    if item then coroutine.yield("then", insertItem, slot, item, item.size) end
    busFree(slot)
    conn.close()
  end
  return "done"
end)

local cycleMiner = (function()
  local isWaiting

  local wait = function()
    cpsWrap(dispatch, function()
      local conn = wrappedConnUtils("miner", 5)
      conn.connect()
      conn.send{op = "stone", slot = isWaiting}
      conn.recv()
      conn.close()
      return "done"
    end)(function()
      busFree(isWaiting)
      isWaiting = nil
    end)
  end

  return cpsWrap(dispatch, function()
    if not isWaiting then
      local recipes = {
        {i = "Stone", o = "Diamond", size = 256},
        {i = "Stone", o = "Emerald", size = 256},
        {i = "Stone", o = "Draconium Ingot", size = 256},
        {i = "Stone", o = "Uranium Ingot", size = 4096},
        {i = "Stone", o = "Iron Ingot", size = 4096},
        {i = "Stone", o = "Gold Ingot", size = 4096},
        {i = "Stone", o = "Tin Ingot", size = 4096},
        {i = "Stone", o = "Lapis Lazuli", size = 4096},
        {i = "Stone", o = "Redstone", size = 4096},
        {i = "Stone", o = "Coal", size = 4096},
        {i = "Stone", o = "Black Quartz", size = 4096},
        {i = "Stone", o = "Certus Quartz Crystal", size = 4096},
        {i = "Stone", o = "thermalfoundation:material;Copper Ingot", size = 4096},
        {i = "Stone", o = "thermalfoundation:material;Silver Ingot", size = 4096},
        {i = "Stone", o = "thermalfoundation:material;Lead Ingot", size = 4096},
        {i = "Stone", o = "thermalfoundation:material;Aluminum Ingot", size = 4096},
        {i = "minecraft:netherrack", o = "Nether Quartz", size = 4096}
      }
      rankRecipes(recipes)
      if recipes[1].demand > 0 then
        isWaiting = true
        local reservation = extractItem(recipes[1].i, 64, "miner")
        isWaiting = coroutine.yield("then", busAllocate)
        coroutine.yield("then", reservation.extract, isWaiting)
        dispatch.queue(wait)
      end
    end
    return "done"
  end)
end)()

-- Main

for i = 1, #gpus do
  gpus[i].proxy.setResolution(table.unpack(gpus[i].resolution))
  gpus[i].proxy.setDepth(gpus[i].proxy.maxDepth())
  gpus[i].proxy.setBackground(0x000000)
end

local cycle
cycle = function()
  local t0 = computer.uptime()
  cpsChain(dispatch,
    cycleStart,
    cycleRunHooks,
    cpsAll(dispatch,
      cyclePulverizer,
      cycleFurnace,
      cycleCobble,
      cycleSandInduction,
      cyclePaper,
      cycleMiner),
    cycleEnd
  )(function()
    dispatch.setAlarm(t0 + 1, cycle)
  end)
end
dispatch.queue(cycle)
dispatch.run()
