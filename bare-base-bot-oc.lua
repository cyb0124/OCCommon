local robot = component.proxy(component.list("robot")())

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
