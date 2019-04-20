local cpsWalk = function(dispatch, actions)
  local result = nil
  for i = 1, #actions do
    local action = string.lower(string.sub(actions, i, i))
    local stepRaw, step = function()
      if action == "f" then
        return robot.move(sides.front)
      elseif action == "b" then
        return robot.move(sides.back)
      elseif action == "u" then
        return robot.move(sides.up)
      elseif action == "d" then
        return robot.move(sides.down)
      elseif action == "l" then
        return robot.turn(false)
      elseif action == "r" then
        return robot.turn(true)
      else
        error("invalid walk action")
      end
    end
    step = function(cont)
      if stepRaw() then
        dispatch.queue(cont)
      else
        computer.beep(880)
        dispatch.queue(function()
          step(cont)
        end)
      end
    end
    if result then
      result = cpsThen(dispatch, result, step)
    else
      result = step
    end
  end
  return result
end

local cpsScopedWalk = function(dispatch, actions, f)
  return cpsChain(dispatch,
    cpsWalk(dispatch, actions),
    f,
    cpsWalk(dispatch, inverseWalkActions(actions))
  )
end
