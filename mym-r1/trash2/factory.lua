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
        timer = dispatch.setTimer(CONNECT_TIMEOUT, function()
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

local me = component.proxy(resolve("me_interface"))
local inv = component.proxy(resolve("transposer"))
local dbAddr = resolve("database")
local db = component.proxy(dbAddr)
local modem = component.proxy(resolve("modem"))
local channel = 1
local hostname = "factory"
local sideME = sides.down
local sideBus = sides.up
local gpus = {
  {proxy = component.proxy(resolve("gpu")), resolution = {80, 25}}
}

-- GPU

local gpuY = 1
local print = function(x, color, beep)
  for i = 1, #gpus do
    if not color then
      color = 0xFFFFFF
    end
    gpus[i].proxy.setForeground(color)
    gpus[i].proxy.set(1, gpuY, x)
  end
  gpuY = gpuY + 1
  if beep then computer.beep(beep) end
end

-- Networking

modem.setStrength(48)
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
        modem.send(remoteUUID, channel,
          json.encode{from = hostname, to = from, rst = true})
      elseif not data.rst then
        IClient(from, remoteUUID)
      end
    elseif to then
      local client = oClients[to]
      if client and from == client.remoteAddr then
        client.remoteUUID = remoteUUID
        client.peer.push(data)
      else
        modem.send(remoteUUID, channel,
          json.encode{from = to, to = from, rst = true})
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

local HandlerStore, HandlerFarmBot, HandlerFert, HandlerCrafter1, HandlerCompost
local HandlerInit = function(iClient)
  local handler, alive = {}, true
  handler.close = function() end
  handler.packet = function(p)
    if p.op == "store" then
      iClient.changeHandler(HandlerStore(iClient, p))
    elseif p.op == "farmBot" then
      iClient.changeHandler(HandlerFarmBot(iClient, p))
    elseif p.op == "fert" then
      iClient.changeHandler(HandlerFert(iClient, p))
    elseif p.op == "crafter1" then
      iClient.changeHandler(HandlerCrafter1(iClient, p))
    elseif p.op == "compost" then
      iClient.changeHandler(HandlerCompost(iClient, p))
    else
      iClient.close()
    end
  end
  return handler
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
      result.changeHandler(HandlerInit(result))
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

local OClient = function(to)
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
  }, false)
  oClients[result.localAddr] = result
  return result
end

local cpsConnect = function(cont, to)
  local result, client, recvBuffer, callbacks, recvCont = {}, OClient(to)
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

local cpsForceGC = function(cont)
  local n, f = 20
  f = function()
    n = n - 1
    if n > 0 then
      dispatch.queue(f)
    else
      cont()
    end
  end
  dispatch.queue(f)
end

local wrappedConnUtils = function(to)
  local result, conn = {}
  result.connect = function()
    if not conn then
      conn = coroutine.yield("then", cpsConnect, to)
    end
  end
  result.send = function(p)
    if not conn then return end
    local result = coroutine.yield("then", conn.send, p)
    if not result then conn = nil end
    return result
  end
  result.recv = function(p)
    if not conn then return end
    local result = coroutine.yield("then", conn.recv)
    if not result then conn = nil end
    return result
  end
  return result
end

-- Bus

local busAllocations = {}
local busWaitQueue = {}
local busUpdate = function()
  local stacks = inv.getAllStacks(sideBus)
  for i = 1, stacks.count() do
    local item = stacks()
    if not busAllocations[i] then
      if not item.name then item.size = 0 end
      if size > 0 then
        inv.transferItem(sideBus, sideME, item.size, i, 9)
        print("Bus: unexpected " .. item.label .. "*" .. item.size .. "@" .. i, 0xFF00000, 660)
        item = inv.getStackInSlot(sideBus, i)
      end
      if size > 0 then
        print("Bus: failed to store the unexpected item", 0xFF00000, 880)
      elseif #busWaitQueue > 0 then
        busAllocations[i] = true
        local callback = table.remove(busWaitQueue)
        local slot = i
        dispatch.queue(function()
          callback(slot)
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
local busAllocateN = function(nRequired, callback)
  local nFinished, slots = 0, {}
  for i = 1, nRequired do
    busAllocate(function(slot)
      nFinished = nFinished + 1
      slots[i] = slot
      if nFinished == nRequired then
        callback(slots)
      end
    end)
  end
end
local busFree = function(slot)
  busAllocations[slot] = nil
  busUpdate()
end

-- Item utilities

local nameMap, labelMap, dualMap, items = {}, {}, {}

local getItem = function(x)
  if type(x) == "string" then
    return nameMap[x] or labelMap[x] or dualMap[x]
  else
    return x
  end
end

local getAvail = function(x)
  x = getItem(x)
  if x then
    return x.size
  else
    return 0
  end
end

local getListAvail = function(itemMap)
  local result = math.huge
  for item, size in pairs(itemMap) do
    result = math.min(result, math.floor(getAvail(item) / size))
  end
  return result
end

local getListSum = function(itemMap)
  local result = 0
  for _, size in pairs(itemMap) do
    result = result + size
  end
  return result
end

local forEachSlot = function(inv, side, f)
  local stacks = inv.getAllStacks(side)
  for slot = 1, stacks.count() do
    local item = stacks()
    if item.name then
      item.size = math.floor(item.size)
      f(item, slot)
    end
  end
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

local reserveItem = function(item, size)
  item = getItem(item)
  size = math.min(getAvail(item), size)
  if size <= 0 then return 0 end
  item.size = item.size - size
  return size
end

local extractFromME = function(item, side, slot, size, reason, alreadyReserved)
  item = getItem(item)
  size = math.min(getAvail(item), size)
  if size <= 0 then return 0 end
  db.clear(1)
  local savedSize = item.size
  item.size = nil
  me.store(item, dbAddr, 1, 1)
  item.size = savedSize
  me.setInterfaceConfiguration(1, dbAddr, 1, size)
  inv.transferItem(sideME, side, size, 1, slot)
  me.setInterfaceConfiguration(1)
  if not alreadyReserved then
    item.size = item.size - size
  end
  if reason then
    reason = reason .. ": "
  else
    reason = ""
  end
  print(reason .. item.label .. "*" .. size)
  return size
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

-- Handlers

local cycleHooks = {}

HandlerStore = function(iClient, p)
  local result, alive, slots = {}, true
  result.close = function()
    alive = false
    if slots then
      for i = 1, #slots do
        local item = inv.getStackInSlot(sideBus, slots[i])
        if item then
          print("From " .. iClient.from .. ": " .. item.label .. "*" .. math.floor(item.size))
          inv.transferItem(sideBus, sideME, item.size, slots[i], 9)
        end
        busFree(slots[i])
      end
    end
  end
  result.packet = function(p)
    if alive then
      iClient.changeHandler(HandlerInit(iClient))
    end
  end
  busAllocateN(p.n, function(result)
    if not alive then
      for i = 1, #result do
        busFree(result[i])
      end
    else
      slots = result
      iClient.peer.send{slots = slots}
    end
  end)
  return result
end

HandlerFarmBot = function(iClient, p)
  local result, alive, slot, cycleCont = {}, true
  result.close = function()
    alive = false
    if slot then busFree(slot) end
    if cycleCont then cycleCont() end
  end
  result.packet = function(p)
    if alive then
      iClient.changeHandler(HandlerInit(iClient))
    end
  end
  table.insert(cycleHooks, function(cont)
    if not alive then cont() return end
    local recipes = {
      {i = "Oak Sapling",    o = "Oak Wood",       size = 4096},
      {i = "Oak Sapling",    o = "Oak Sapling",    size = 64, s = true},
      {i = "Spruce Sapling", o = "Spruce Wood",    size = 4096},
      {i = "Spruce Sapling", o = "Spruce Sapling", size = 64, s = true}
    }
    rankRecipes(recipes, function(recipe)
      if recipe.s and recipe.demand > 0 then
        recipe.demand = 2
      end
    end)
    local recipe = recipes[1]
    if recipe.demand <= 0 then
      iClient.peer.send{}
      cont()
    else
      cycleCont = cont
      busAllocate(function(result)
        if not alive then
          busFree(result)
        else
          slot = result
          extractFromME(recipe.i, sideBus, slot, 8, "farmBot input")
          iClient.peer.send{slot = slot}
          cycleCont = nil
          cont()
        end
      end)
    end
  end)
  return result
end

HandlerCrafter1 = function(iClient, p)
  local result, alive, slots, cycleCont = {}, true
  result.close = function()
    alive = false
    if slots then for _, slot in ipairs(slots) do busFree(slot) end end
    if cycleCont then cycleCont() end
  end
  result.packet = function(p)
    if alive then
      iClient.changeHandler(HandlerInit(iClient))
    end
  end
  table.insert(cycleHooks, function(cont)
    if not alive then cont() return end
    local recipes = {
      {is = {["minecraft:planks;Oak Wood Planks"] = 2},  o = "minecraft:stick", size = 64, batchSize = 4},
      {is = {["minecraft:stick"] = 1, ["Leather"] = 1},  o = "Knife Handle",    size = 1,  batchSize = 1},
      {is = {["Iron Ingot"] = 2, ["Flint"] = 1},         o = "Knife Blade",     size = 1,  batchSize = 1},
      {is = {["Knife Handle"] = 1, ["Knife Blade"] = 1}, o = "Knife",           size = 1,  batchSize = 1},
      {is = {["Knife"] = 1, ["Cooked Fish"] = 8},        o = "Bio-Mash",        size = 64, batchSize = 1}
    }
    rankRecipes(recipes)
    local recipe = recipes[1]
    if recipe.demand <= 0 then
      iClient.peer.send{}
      cont()
    else
      cycleCont = cont
      local nSlots = 0 for _, _ in pairs(recipe.is) do nSlots = nSlots + 1 end
      busAllocateN(nSlots, function(result)
        if not alive then
          for _, slot in ipairs(result) do busFree(result) end
        else
          recipe.ia = getListAvail(recipe.is)
          if recipe.ia <= 0 then
            for _, slot in ipairs(result) do busFree(result) end
            iClient.peer.send{}
          else
            slots = result
            local batchSize = math.min(recipe.ia, recipe.batchSize)
            local i = 1
            for item, size in pairs(recipe.is) do
              extractFromME(item, sideBus, slots[i], size * batchSize, "crafter1 input")
              i = i + 1
            end
            iClient.peer.send{slots = slots}
          end
          cycleCont = nil
          cont()
        end
      end)
    end
  end)
  return result
end

HandlerCompost = function(iClient, p)
  local result, alive, slot, cycleCont = {}, true
  result.close = function()
    alive = false
    if slot then busFree(slot) end
    if cycleCont then cycleCont() end
  end
  result.packet = function(p)
    if alive then
      iClient.changeHandler(HandlerInit(iClient))
    end
  end
  table.insert(cycleHooks, function(cont)
    if not alive then cont() return end
    local avail = getAvail("actuallyadditions:item_fertilizer")
    local toproc = math.min(256 - avail - p.n, 64 - p.n, getAvail("Bio-Mash"))
    if toproc <= 0 then
      iClient.peer.send{}
      cont()
    else
      cycleCont = cont
      busAllocate(function(result)
        if not alive then
          busFree(result)
        else
          slot = result
          extractFromME("Bio-Mash", sideBus, slot, toproc, "compost")
          iClient.peer.send{slot = slot}
          cycleCont = nil
          cont()
        end
      end)
    end
  end)
  return result
end

HandlerFert = function(iClient, p)
  local result, alive, slot, cycleCont = {}, true
  result.close = function()
    alive = false
    if slot then busFree(slot) end
    if cycleCont then cycleCont() end
  end
  result.packet = function(p)
    if alive then
      iClient.changeHandler(HandlerInit(iClient))
    end
  end
  table.insert(cycleHooks, function(cont)
    if not alive then cont() return end
    local fert = getItem "actuallyadditions:item_fertilizer"
    if getAvail(fert) <= 0 then
      iClient.peer.send{}
      cont()
    else
      cycleCont = cont
      busAllocate(function(result)
        if not alive then
          busFree(result)
        else
          slot = result
          extractFromME(fert, sideBus, slot, 8, "fert input")
          iClient.peer.send{slot = slot}
          cycleCont = nil
          cont()
        end
      end)
    end
  end)
  return result
end

-- Cycle

local cycleCounter = 1

local cycleStart = function(cont)
  for i = 1, #gpus do
    local resolution = gpus[i].resolution
    gpus[i].proxy.fill(1, 1, resolution[1], resolution[2], " ")
  end
  gpuY = 1
  print("factory-controller by cybcaoyibo - Cycle #" .. cycleCounter, 0x7FFF00)
  items = me.getItemsInNetwork()
  for i = 1, #items do
    local item = items[i]
    item.size = math.floor(item.size)
    nameMap[item.name] = item
    labelMap[item.label] = item
    dualMap[item.name .. ";" .. item.label] = item
  end
  cpsForceGC(function()
    print("Mem: " .. toKB(computer.freeMemory())
      .. "/" .. toKB(computer.totalMemory()))
    cont()
  end)
end

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

local cycleTemplateTE = function(name, side, recipes)
  return function()
    local maxInproc = 48
    local inproc = 0
    local inprocName
    forEachSlot(inv, side, function(item, slot)
      inproc = inproc + item.size
      if slot > 1 then
        print(name .. " output: " .. item.label .. "*" .. item.size)
        inv.transferItem(side, sideME, item.size, slot, 9)
      else
        inprocName = item.name
      end
    end)
    if inproc < maxInproc then
      rankRecipes(recipes)
      for i = 1, #recipes do
        local recipe = recipes[i]
        if recipe.demand <= 0 then break end
        if not inprocName or inprocName == recipe.i then
          inproc = inproc + extractFromME(recipe.i, side, 1,
            math.min(maxInproc - inproc, recipe.size - recipe.oa - inproc),
            name .. " input")
          break
        end
      end
    end
  end
end

local cyclePulverizer = cycleTemplateTE("Pulverizer", sides.zp, {
  {i = "minecraft:cobblestone", o = "minecraft:sand", size = 4096}
})

local cycleFurnace = cycleTemplateTE("Furnace", sides.zn, {
  {i = "minecraft:sand", o = "minecraft:glass", size = 4096},
  {i = "Raw Fish",       o = "Cooked Fish",     size = 256}
})

local cycleSawmill = cycleTemplateTE("Sawmill", sides.xp, {
  {i = "Oak Wood", o = "minecraft:planks;Oak Wood Planks", size = 1024}
})

local cycleCobble = function()
  local rs = component.proxy(resolve("23395155"))
  local side = sides.zn
  local toStock = 4096
  local avail = getAvail("minecraft:cobblestone")
  if avail < toStock then
    print("Cobble: " .. avail .. "/" .. toStock)
    rs.setOutput(side, 15)
  else
    rs.setOutput(side, 0)
  end
end

local cycleEnd = function()
  nameMap, labelMap, dualMap, items = {}, {}, {}
  busUpdate()
  cycleCounter = cycleCounter + 1
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
  cpsChain(dispatch,
    cycleStart,
    cycleRunHooks,
    cpsAll(dispatch,
      cps(cyclePulverizer),
      cps(cycleFurnace),
      cps(cycleSawmill),
      cps(cycleCobble)),
    cps(cycleEnd)
  )(function()
    dispatch.setAlarm(t0 + 1, cycle)
  end)
end
dispatch.queue(cycle)
dispatch.run()
