local robot = component.proxy(component.list("robot")())
local inv = component.proxy(component.list("inventory_controller")())

local waitUntil = function(t1)
  while true do
    local td = t1 - computer.uptime()
    if td <= 0 then break end
    computer.pullSignal(td)
  end
end

local sleep = function(duration)
  waitUntil(computer.uptime() + duration)
end

local getEnergy = function()
  return computer.energy() / computer.maxEnergy()
end

local retrying = function(...)
  while not ({...})[1](select(2, ...)) do
    computer.beep(880)
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

local walk = function(actions)
  for i = 1, #actions do
    local action = string.lower(string.sub(actions, i, i))
    if action == "f" then
      retrying(robot.move, sides.front)
    elseif action == "b" then
      retrying(robot.move, sides.back)
    elseif action == "u" then
      retrying(robot.move, sides.up)
    elseif action == "d" then
      retrying(robot.move, sides.down)
    elseif action == "l" then
      retrying(robot.turn, false)
    elseif action == "r" then
      retrying(robot.turn, true)
    else
      error("invalid walk action")
    end
  end
end

local inverseWalkActions = function(input)
  local output = ""
  input = string.reverse(input)
  for i = 1, #input do
    local now = string.lower(string.sub(input, i, i))
    if now == "f" then
      output = output .. "b"
    elseif now == "b" then
      output = output .. "f"
    elseif now == "u" then
      output = output .. "d"
    elseif now == "d" then
      output = output .. "u"
    elseif now == "l" then
      output = output .. "r"
    elseif now == "r" then
      output = output .. "l"
    else
      output = output .. now
    end
  end
  return output
end

local scopedWalk = function(actions, f)
  walk(actions)
  f()
  walk(inverseWalkActions(actions))
end

while true do
  local t0 = computer.uptime()

  if getEnergy() < 0.2 then
    scopedWalk("LFFFF", function()
      while getEnergy() < 0.8 do sleep(0.2) end
    end)
  end

  for i = 1, 16 do robot.suck(sides.front) end

  local slotsDamaged = {}
  local slotsBook = {}
  local slotsEnchanted = {}
  local slotsOther = {}

  for i = 1, 16 do
    local item = inv.getStackInInternalSlot(i)
    if item then
      if item.maxDamage > 0 and item.damage > 0 then
        table.insert(slotsDamaged, i)
      elseif item.name == "minecraft:enchanted_book" then
        table.insert(slotsBook, i)
      elseif item.enchantments then
        table.insert(slotsEnchanted, i)
      else
        table.insert(slotsOther, i)
      end
    end
  end

  local walkAndDrop = function(actions, slots)
    scopedWalk(actions, function()
      for _, i in pairs(slots) do
        robot.select(i)
        robot.drop(sides.front)
      end
    end)
  end

  if #slotsOther > 0 then walkAndDrop("LFFR", slotsOther) end
  if #slotsBook > 0 then walkAndDrop("LLFFLF", slotsBook) end
  if #slotsEnchanted > 0 then walkAndDrop("LLFFR", slotsEnchanted) end
  if #slotsDamaged > 0 then walkAndDrop("LLFFFFR", slotsDamaged) end

  waitUntil(t0 + 1)
end
