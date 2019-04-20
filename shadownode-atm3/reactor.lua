--big reactor controller script (for oc)
--by cybcaoyibo

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

local config = {
  gpus = {{proxy = component.proxy(resolve("gpu")), resolution = {160, 50}}},
  reactor = "br_reactor",
  tuningDisk = component.proxy(resolve("0b4")),
  tuningName = "bigreactors-tuning.lua"
}

local pc = function(short, ...)
  local resolved = resolve(short)
  if not resolved then return end
  local rst = {pcall(component.invoke, resolved, ...)}
  if not rst[1] then return end
  return select(2, table.unpack(rst))
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

local round = function(num)
  return math.floor(num + 0.5)
end

local toPercentage = function(num)
  return tostring(round(num * 1000) / 10)
end

local toKB = function(num)
  return tostring(round(num / 1024))
end

local getPV, hasTurbine
if not config.turbines or #(config.turbines) == 0 then
  hasTurbine = false
  getPV = function()
    local energy = pc(config.reactor, "getEnergyStored")
    if not energy then return nil end
    return energy / 10000000
  end
else
  hasTurbine = true
  getPV = function()
    local steam = pc(config.reactor, "getHotFluidAmount")
    local steamMax = pc(config.reactor, "getHotFluidAmountMax")
    if not steam or not steamMax then return nil end
    return steam / steamMax
  end
end

local tuning
local savedTuning = (function()
  local fd = config.tuningDisk.open(config.tuningName, "r")
  if not fd then return nil end
  local buffer = ""
  while true do
    local now = config.tuningDisk.read(fd, 4096)
    if not now then break end
    buffer = buffer .. now
  end
  config.tuningDisk.close(fd)
  return load(buffer)
end)()
if savedTuning ~= nil then
  tuning = savedTuning()
else
  tuning = {kP = 0, kI = 0, kD = 0, ts = 10}
end

local saveTuning = function()
  local data = "return {kP = " .. tuning.kP .. ", kI = " .. tuning.kI .. ", kD = " .. tuning.kD .. ", ts = " .. tuning.ts .. "}\n"
  local fd = config.tuningDisk.open(config.tuningName, "w")
  if fd then
    config.tuningDisk.write(fd, data)
    config.tuningDisk.close(fd)
  end
end

local getP = function() return tuning.kP end
local getI = function() return tuning.kP * tuning.kI end
local getD = function() return tuning.kP * tuning.kD end
local getTS = function() return tuning.ts / 20 end

local selectedTuning = "kP"
local nowE, out, accum, prevE = 0, 0, 0, 0
local turbineStates = {}
local lastError = "INIT"

local control = function()
  lastError = nil
  local pv = getPV()
  if not pv then lastError = "PV" return end
  nowE = (0.5 - pv) * 2
  accum = accum + getTS() * nowE
  if accum > 1 / getI() then accum = 1 / getI()
  elseif accum < -1 / getI() then accum = -1 / getI() end
  local diff = (nowE - prevE) / getTS()
  prevE = nowE
  out = nowE * getP() + accum * getI() + diff * getD()
  out = 0.5 - out
  if out > 1 then
    out = 1
    pc(config.reactor, "setActive", false)
  else
    pc(config.reactor, "setActive", true)
  end
  if out < 0 then out = 0 end

  local nRods = pc(config.reactor, "getNumberOfControlRods")
  if not nRods then lastError = "nRods" return end
  local scaledOut = out * 100
  local nFine = round(scaledOut * nRods) - math.floor(scaledOut) * nRods
  local baseOut = math.floor(scaledOut)

  local rodMap = {}
  for i = 1, nRods do
    rodMap[i] = i
  end

  for i = 1, nRods - 1 do
    local with = math.random(i, nRods)
    local tmp = rodMap[i]
    rodMap[i] = rodMap[with]
    rodMap[with] = tmp
  end

  for i = 1, nRods do
    local nowOut = baseOut
    if i <= nFine then nowOut = nowOut + 1 end
    pc(config.reactor, "setControlRodLevel", rodMap[i] - 1, nowOut)
  end

  if hasTurbine then
    for i = 1, #(config.turbines) do
      local turbine = config.turbines[i]
      local state = {}
      turbineStates[i] = state
      state.speed = pc(turbine.name, "getRotorSpeed")
      state.active = pc(turbine.name, "getActive")
      if not state.speed or not state.active then lastError = "RPM#" .. tostring(i) return end
      state.engage = state.speed >= turbine.rpm
      pc(turbine.name, "setInductorEngaged", state.engage)
    end
  end
end

local gpus = {}
local turbineWidth = 10

for i = 1, #(config.gpus) do
  local proxy = config.gpus[i].proxy
  local resolution = config.gpus[i].resolution

  local bufferOld, bufferNew, nowFG, nowBG, lastFG, lastBG
  local newBuffer = function()
    local topBar = {}
    local rightBar = {}
    local leftBar = {}
    local base = function(x, y)
      if y == 1 then
        return topBar, (x - 1) * 3
      elseif x <= turbineWidth then
        return leftBar, ((x - 1) * (resolution[2] - 1) + (y - 2)) * 3
      else
        return rightBar, (y - 2) * 3
      end
    end
    return {
      set = function(x, y, c, fg, bg)
        local theBuffer, theBase = base(x, y)
        theBuffer[theBase + 1] = string.byte(c)
        theBuffer[theBase + 2] = fg
        theBuffer[theBase + 3] = bg
      end,
      get = function(x, y)
        local theBuffer, theBase = base(x, y)
        return
          string.char(theBuffer[theBase + 1]),
          theBuffer[theBase + 2],
          theBuffer[theBase + 3]
      end
    }
  end

  local getMaxY = function(x)
    if x <= turbineWidth or x == resolution[1] then
      return resolution[2]
    else
      return 1
    end
  end

  local clearBuffer = function(buffer)
    for x = 1, resolution[1] do
      for y = 1, getMaxY(x) do
        buffer.set(x, y, " ", 0xFFFFFF, 0x000000)
      end
    end
  end

  local swapBuffers = function()
    local temp = bufferOld
    bufferOld = bufferNew
    bufferNew = temp
  end

  bufferOld = newBuffer()
  bufferNew = newBuffer()
  clearBuffer(bufferOld)
  clearBuffer(bufferNew)

  table.insert(gpus, {
    reset = function()
      proxy.setResolution(resolution[1], resolution[2])
      proxy.setDepth(proxy.maxDepth())
      proxy.setBackground(0x000000)
      proxy.fill(1, 1, resolution[1], resolution[2], " ")
      lastBG = 0x000000
    end,
    startDraw = function()
      clearBuffer(bufferNew)
    end,
    getResolution = function()
      return resolution[1], resolution[2]
    end,
    setForeground = function(x)
      nowFG = x
    end,
    setBackground = function(x)
      nowBG = x
    end,
    set = function(x, y, s)
      for i = 1, #s do
        local nowX = x + i - 1
        if nowX <= resolution[1] then
          bufferNew.set(nowX, y, string.sub(s, i, i), nowFG, nowBG)
        end
      end
    end,
    endDraw = function(scroll)
      if scroll then
        proxy.copy(turbineWidth + 1, 2,
          resolution[1] - turbineWidth, resolution[2] - 1, -1, 0)
        proxy.copy(1, 2, 1, resolution[2] - 1, turbineWidth - 1, 0)
        for y = 2, resolution[2] do
          bufferOld.set(turbineWidth, y, bufferOld.get(1, y))
        end
      end
      for x = 1, resolution[1] do
        for y = 1, getMaxY(x) do
          local cNew, fNew, bNew = bufferNew.get(x, y)
          local cOld, fOld, bOld = bufferOld.get(x, y)
          if cNew ~= cOld or fNew ~= fOld or bNew ~= bOld then
            if lastFG ~= fNew then
              lastFG = fNew
              proxy.setForeground(lastFG)
            end
            if lastBG ~= bNew then
              lastBG = bNew
              proxy.setBackground(lastBG)
            end
            proxy.set(x, y, cNew)
          end
        end
      end
      swapBuffers()
    end
  })
end

local redraw = function(scroll)
  for i = 1, #gpus do
    gpus[i].startDraw()
    -- kP
    if selectedTuning == "kP" then
      gpus[i].setForeground(0xFFFFFF)
      gpus[i].setBackground(0x008000)
    else
      gpus[i].setForeground(0x00FF00)
      gpus[i].setBackground(0x000000)
    end
    gpus[i].set(1, 1, toPercentage(tuning.kP))

    -- kI
    if selectedTuning == "kI" then
      gpus[i].setForeground(0xFFFFFF)
      gpus[i].setBackground(0xFF0000)
    else
      gpus[i].setForeground(0xFF0000)
      gpus[i].setBackground(0x000000)
    end
    gpus[i].set(8, 1, toPercentage(tuning.kI))

    -- kD
    if selectedTuning == "kD" then
      gpus[i].setForeground(0xFFFFFF)
      gpus[i].setBackground(0x007FFF)
    else
      gpus[i].setForeground(0x007FFF)
      gpus[i].setBackground(0x000000)
    end
    gpus[i].set(14, 1, toPercentage(tuning.kD))

    -- ts
    if selectedTuning == "ts" then
      gpus[i].setForeground(0xFFFFFF)
      gpus[i].setBackground(0xFFA500)
    else
      gpus[i].setForeground(0xFFA500)
      gpus[i].setBackground(0x000000)
    end
    gpus[i].set(20, 1, tostring(tuning.ts))

    gpus[i].setForeground(0xFFFFFF)
    gpus[i].setBackground(0x000000)
    gpus[i].set(25, 1, "bigreactors-controller by cybcaoyibo")

    gpus[i].set(1, 2, toKB(computer.freeMemory())
      .. "/" .. toKB(computer.totalMemory()))

    if lastError ~= nil then
      gpus[i].setForeground(0xFFFFFF)
      gpus[i].setBackground(0xFF0000)
      gpus[i].set(1, 3, "ERR: " .. lastError)
    else
      -- Turbines
      if hasTurbine then
        for j = 1, #turbineStates do
          local xpos = 1
          local w = function(x)
            gpus[i].set(xpos, j + 2, x)
            xpos = xpos + #x
          end
          gpus[i].setBackground(0x000000)
          if turbineStates[j].engage then gpus[i].setForeground(0x00FF00)
          else gpus[i].setForeground(0xFF0000) end
          w(tostring(round(turbineStates[j].speed)))
          gpus[i].setForeground(0xFFFFFF)
          w("/")
          if turbineStates[j].active then gpus[i].setForeground(0x00FF00)
          else gpus[i].setForeground(0xFF0000) end
          w(tostring(round(config.turbines[j].rpm)))
        end
      else
        gpus[i].setForeground(0xFFFFFF)
        gpus[i].setBackground(0x000000)
        gpus[i].set(1, 3, "PASSIVE")
      end

      -- Chart
      local width, height = gpus[i].getResolution()
      local getY = function(value)
        if value > 1 then value = 1
        elseif value < -1 then value = -1 end
        return round((1 - height / 2) * value + height / 2 + 1)
      end
      local drawValue = function(value, color, chr)
        local y = getY(value)
        if y == getY(0) then
          gpus[i].setBackground(0x007F00)
        else
          gpus[i].setBackground(0x000000)
        end
        gpus[i].setForeground(color)
        gpus[i].set(width, y, chr)
      end
      drawValue(0, 0x00FF00, "-")
      drawValue(1 - out * 2, 0xFFA500, "O")
      drawValue(accum * getI(), 0xFF0000, "I")
      drawValue(-nowE, 0x00FFFF, "E")
    end

    gpus[i].endDraw(scroll)
  end
end

local dispatch
dispatch = Dispatch(function(ev, kbdAddr, char, code, player)
  if ev == "key_down" then
    if code == 0x19 then
      selectedTuning = "kP"
      redraw(false)
    elseif code == 0x17 then
      selectedTuning = "kI"
      redraw(false)
    elseif code == 0x20 then
      selectedTuning = "kD"
      redraw(false)
    elseif code == 0x14 then
      selectedTuning = "ts"
      redraw(false)
    elseif code == 0xC8 then
      if selectedTuning == "ts" then
        tuning.ts = tuning.ts + 1
        if tuning.ts > 20 then tuning.ts = 20 end
      elseif selectedTuning == "kP" then
        if tuning[selectedTuning] >= 1 then
          tuning[selectedTuning] = tuning[selectedTuning] + 1/2
        else
          tuning[selectedTuning] = tuning[selectedTuning] + 1/128
        end
        if tuning[selectedTuning] > 64 then tuning[selectedTuning] = 64 end
      else
        tuning[selectedTuning] = tuning[selectedTuning] + 1/128
        if tuning[selectedTuning] > 1 then tuning[selectedTuning] = 1 end
      end
      saveTuning()
      redraw(false)
    elseif code == 0xD0 then
      if selectedTuning == "ts" then
        tuning.ts = tuning.ts - 1
        if tuning.ts < 1 then tuning.ts = 1 end
      elseif selectedTuning == "kP" then
        if tuning[selectedTuning] > 1 then
          tuning[selectedTuning] = tuning[selectedTuning] - 1/2
        else
          tuning[selectedTuning] = tuning[selectedTuning] - 1/128
        end
      else
        tuning[selectedTuning] = tuning[selectedTuning] - 1/128
      end
      if tuning[selectedTuning] < 0 then tuning[selectedTuning] = 0 end
      saveTuning()
      redraw(false)
    end
  end
end)

local theTimer
local setTimer
setTimer = function()
  local newTimer
  newTimer = dispatch.setTimer(getTS(), function()
    if theTimer == newTimer then
      setTimer()
      control()
      redraw(true)
    end
  end)
  theTimer = newTimer
end

setTimer()
for i = 1, #gpus do
  gpus[i].reset()
end
redraw(false)
dispatch.run()
