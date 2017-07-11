--big reactor controller script
--by cybcaoyibo

local unpack = function(tab)
	if table.unpack ~= nil then return table.unpack(tab) end
	return _G.unpack(tab)
end

local pack = function(...)
	return {...}
end

local pc = function(...)
	local rst = pack(pcall(peripheral.call, unpack({...})))
	if rst[1] == false then return nil end
	return select(2, unpack(rst))
end

local round = function(num)
  return math.floor(num + 0.5)
end

local toPercentage = function(num)
  return tostring(round(num * 1000) / 10)
end

local config = (loadfile "br-config.lua")()

local getPV, hasTurbine
if config.turbines == nil or #(config.turbines) == 0 then
  hasTurbine = false
  getPV = function()
    local energy = pc(config.reactor, "getEnergyStored")
    if energy == nil then return nil end
    return energy / 10000000
  end
else
  hasTurbine = true
  getPV = function()
    local steam = pc(config.reactor, "getHotFluidAmount")
    local steamMax = pc(config.reactor, "getHotFluidAmountMax")
    if steam == nil or steamMax == nil then return nil end
    return steam / steamMax
  end
end

local tuning
local savedTuning = loadfile "br-tuning.lua"
if savedTuning ~= nil then
  tuning = savedTuning()
else
  tuning = {kP = 0, kI = 0, kD = 0, ts = 10}
end

local saveTuning = function()
  local f = io.open("br-tuning.lua", "w")
  f:write("return {kP = " .. tuning.kP .. ", kI = " .. tuning.kI .. ", kD = " .. tuning.kD .. ", ts = " .. tuning.ts .. "}\n")
  f:close()
end

local getP = function() return tuning.kP end
local getI = function() return tuning.kP * tuning.kI end
local getD = function() return tuning.kP * tuning.kD end
local getTS = function() return tuning.ts / 20 end

local selectedTuning = "kP"
local nowE, out, accum, prevE = 0, 0, 0, 0
local turbineStates = {}
local lastError = "INIT"
local terms = {term}
local historyForTerm = {{}}

local control = function()
  lastError = nil
  local pv = getPV()
  if pv == nil then lastError = "PV" return end
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
  if nRods == nil then lastError = "nRods" return end
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
      if state.speed == nil or state.active == nil then lastError = "RPM#" .. tostring(i) return end
      state.engage = state.speed >= turbine.rpm
      pc(turbine.name, "setInductorEngaged", state.engage)
    end
  end
  
  for i = 1, #terms do
    local history = historyForTerm[i]
    table.insert(history, 1, {nowE = nowE, out = out, accum = accum})
  end
end

for i = 1, #(config.monitors) do
  local name = config.monitors[i].name
  local scale = config.monitors[i].scale
  table.insert(terms, setmetatable({}, {__index = function(tab, key)
    return function(...)
      if key == "clear" then pc(name, "setTextScale", scale) end
      return pc(name, key, ...)
    end
  end}))
  table.insert(historyForTerm, {})
end

local clearScreen = function()
  for i = 1, #terms do
    terms[i].setCursorBlink(false)
    terms[i].setBackgroundColor(colors.black)
    terms[i].clear()
    terms[i].setCursorPos(1, 1)
  end
end

local redraw = function()
  clearScreen()
  for i = 1, #terms do
    -- kP
    if selectedTuning == "kP" then
      terms[i].setTextColor(colors.white)
      terms[i].setBackgroundColor(colors.green)
    else
      terms[i].setTextColor(colors.green)
      terms[i].setBackgroundColor(colors.black)
    end
    terms[i].write(toPercentage(tuning.kP))
    
    -- kI
    terms[i].setCursorPos(8, 1)
    if selectedTuning == "kI" then
      terms[i].setTextColor(colors.white)
      terms[i].setBackgroundColor(colors.red)
    else
      terms[i].setTextColor(colors.red)
      terms[i].setBackgroundColor(colors.black)
    end
    terms[i].write(toPercentage(tuning.kI))
    
    -- kD
    terms[i].setCursorPos(14, 1)
    if selectedTuning == "kD" then
      terms[i].setTextColor(colors.white)
      terms[i].setBackgroundColor(colors.blue)
    else
      terms[i].setTextColor(colors.blue)
      terms[i].setBackgroundColor(colors.black)
    end
    terms[i].write(toPercentage(tuning.kD))
    
    -- ts
    terms[i].setCursorPos(20, 1)
    if selectedTuning == "ts" then
      terms[i].setTextColor(colors.white)
      terms[i].setBackgroundColor(colors.orange)
    else
      terms[i].setTextColor(colors.orange)
      terms[i].setBackgroundColor(colors.black)
    end
    terms[i].write(tostring(tuning.ts))
    
    terms[i].setCursorPos(25, 1)
    terms[i].setTextColor(colors.white)
    terms[i].setBackgroundColor(colors.black)
    terms[i].write("br-ctrl by cybcaoyibo")
    
    if lastError ~= nil then
      terms[i].setCursorPos(1, 2)
      terms[i].setTextColor(colors.white)
      terms[i].setBackgroundColor(colors.red)
      terms[i].write("ERROR: " .. lastError)
    else
      -- Turbines
      local turbineWidth
      if hasTurbine then
        turbineWidth = 10
        for j = 1, #turbineStates do
          terms[i].setCursorPos(1, j + 1)
          terms[i].setBackgroundColor(colors.black)
          if turbineStates[j].engage then terms[i].setTextColor(colors.green)
          else terms[i].setTextColor(colors.red) end
          terms[i].write(tostring(round(turbineStates[j].speed)))
          terms[i].setTextColor(colors.white)
          terms[i].write("/")
          if turbineStates[j].active then terms[i].setTextColor(colors.green)
          else terms[i].setTextColor(colors.red) end
          terms[i].write(tostring(round(config.turbines[j].rpm)))
        end
      else
        turbineWidth = 8
        terms[i].setCursorPos(1, 2)
        terms[i].setTextColor(colors.white)
        terms[i].setBackgroundColor(colors.black)
        terms[i].write("PASSIVE")
      end
      
      -- Chart
      local width, height = terms[i].getSize()
      if width ~= nil and height ~= nil then
        local getY = function(value)
          if value > 1 then value = 1
          elseif value < -1 then value = -1 end
          return round((1 - height / 2) * value + height / 2 + 1)
        end
        local drawValue = function(x, value, color, chr)
          local y = getY(value)
          terms[i].setCursorPos(x, y)
          if y == getY(0) then
            terms[i].setBackgroundColor(colors.green)
          else
            terms[i].setBackgroundColor(colors.black)
          end
          terms[i].setTextColor(color)
          terms[i].write(chr)
        end
        local drawAtX = function(x, nowE, out, accum)
          drawValue(x, 0, colors.green, "-")
          drawValue(x, 1 - out * 2, colors.orange, "O")
          drawValue(x, accum * getI(), colors.red, "I")
          drawValue(x, -nowE, colors.lightBlue, "E")
        end
        
        local history = historyForTerm[i]
        while #history > (width - turbineWidth) do table.remove(history) end
        for i = 1, #history do
          drawAtX(width - i + 1, history[i].nowE, history[i].out, history[i].accum)
        end
      end
    end
  end
end

local timer = os.startTimer(getTS())
redraw()

while true do
  local ev, eArg = os.pullEvent()
  if ev == "timer" and eArg == timer then
    timer = os.startTimer(getTS())
    control()
    redraw()
  elseif ev == "key" then
    if eArg == keys.x then
      break
    elseif eArg == keys.p then
      selectedTuning = "kP"
      redraw()
    elseif eArg == keys.i then
      selectedTuning = "kI"
      redraw()
    elseif eArg == keys.d then
      selectedTuning = "kD"
      redraw()
    elseif eArg == keys.t then
      selectedTuning = "ts"
      redraw()
    elseif eArg == keys.up then
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
      redraw()
    elseif eArg == keys.down then
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
      redraw()
    end
  end
end

os.cancelTimer(timer)
clearScreen()
