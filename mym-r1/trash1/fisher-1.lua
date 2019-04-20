local computer = require "computer"
local robot = require "robot"
local component = require "component"

local repairMap = {
  ["minecraft:fishing_rod"] = 4,
  ["minecraft:leather_boots"] = 8,
  ["minecraft:bow"] = 12
}

local function getEnergy()
  return computer.energy() / computer.maxEnergy()
end

local function retrying(...)
  while not ({...})[1](select(2, ...)) do os.sleep(0.1) end
end

local function drop()
  retrying(robot.turnRight)
  retrying(robot.turnRight)
  retrying(robot.drop)
  retrying(robot.turnRight)
  retrying(robot.turnRight)
end

while true do
  if getEnergy() < 0.2 then
    print("recharging")
    retrying(robot.turnLeft)
    retrying(robot.forward)
    while getEnergy() < 0.8 do os.sleep(0.1) end
    retrying(robot.back)
    retrying(robot.turnRight)
  end

  robot.select(16)
  if robot.count() > 0 or robot.suck() then
    local info = component.inventory_controller.getStackInInternalSlot(16)
    print(info.name .. "*" .. info.size)
    local repairSlot = repairMap[info.name]
    if repairSlot == nil or info.damage == 0 then
      drop()
    else
      if robot.count(repairSlot) > 0 then
        print("repairing")
        retrying(robot.transferTo, 1)
        robot.select(repairSlot)
        retrying(robot.transferTo, 2)
        robot.select(16)
        retrying(component.crafting.craft)
        drop()
      else
        print("stored for repair")
        retrying(robot.transferTo, repairSlot)
      end
    end
  else
    os.sleep(0.5)
  end
end
