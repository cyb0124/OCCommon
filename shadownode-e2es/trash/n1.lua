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

local TIMEOUT = 3
local NAGLING = 0.1
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

local inv1 = component.proxy(resolve("eb7"))
local inv2 = component.proxy(resolve("fe6"))
local modem = component.proxy(resolve("modem"))
local channel = 1
local hostname = "n1"

-- Networking

if modem.setStrength then modem.setStrength(math.huge) end
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
  local result = {remoteUUID = initRemoteUUID, from = from}
  result.close = function()
    result.peer.activeClose()
    result.changeHandler(nil)
    iClients[from] = nil
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

local getInproc = function(inv, side)
  local inproc, filter = 0
  forEachSlot(inv, side, function(item, slot)
    inproc = inproc + item.size
    if slot == 1 then filter = item.label end
  end)
  return {inproc = inproc, filter = filter}
end

-- Handlers

cpsHandleInit = function(conn)
  while true do
    local p = conn.recv()
    if not p then return "done" end
    if p.op == "furnace" then
      conn.send(getInproc(inv1, sides.up))
      p = conn.recv()
      if p and p.slot then
        inv1.transferItem(sides.south, sides.up, p.size, p.slot, 1)
        conn.send{}
      end
    elseif p.op == "manufactory" then
      conn.send(getInproc(inv1, sides.down))
      p = conn.recv()
      if p and p.slot then
        inv1.transferItem(sides.south, sides.down, p.size, p.slot, 1)
        conn.send{}
      end
    elseif p.op == "squeezer" then
      local stat = getInproc(inv2, sides.down)
      forEachSlot(inv2, sides.north, function(item) stat.inproc = stat.inproc + item.size end)
      conn.send(stat)
      p = conn.recv()
      if p and p.slot then
        inv2.transferItem(sides.north, sides.down, p.size, p.slot, 1)
        conn.send{}
      end
    elseif p.op == "cobbleGen" then
      inv2.transferItem(sides.up, sides.north, p.size, 1, p.slot)
      conn.send{}
    else
      conn.close()
    end
  end
end

-- Main

dispatch.queue(cpsWrap(function()
  local conn = wrappedConnUtils("factory")
  while true do
    local t0, p = computer.uptime()
    conn.connect()

    local storeActions = {}
    -- Furnace Output
    forEachSlot(inv1, sides.up, function(item, slot)
      if slot > 1 then table.insert(storeActions, function(busSlot) inv1.transferItem(sides.up, sides.south, item.size, slot, busSlot) end) end
    end)
    -- Manufactory Output
    forEachSlot(inv1, sides.down, function(item, slot)
      if slot > 1 then table.insert(storeActions, function(busSlot) inv1.transferItem(sides.down, sides.south, item.size, slot, busSlot) end) end
    end)
    -- Squeezer Output
    forEachSlot(inv2, sides.south, function(item, slot)
      table.insert(storeActions, function(busSlot) inv2.transferItem(sides.south, sides.north, item.size, slot, busSlot) end)
    end)
    if #storeActions > 0 then
      conn.send{op = "store", n = #storeActions}
      p = conn.recv()
      if p then
        for i, slot in ipairs(p.slots) do storeActions[i](slot) end
        conn.send{}
      end
    end

    coroutine.yield("then", function(c) dispatch.setAlarm(t0 + 1, c) end)
  end
end))

dispatch.run()
