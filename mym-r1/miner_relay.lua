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

local channel = 1
local hostWired = "factory"
local hostWireless = "miner"
local strength = 16

local modemWired, modemWireless, uuidWired, uuidWireless
for addr in component.list("modem") do
  local proxy = component.proxy(addr)
  if proxy.isWireless() then
    modemWireless = proxy
  else
    modemWired = proxy
  end
end
modemWireless.setStrength(strength)
modemWired.open(channel)
modemWireless.open(channel)
Dispatch(function(kind, _, uuidFrom, _, _, data, from, to)
  if kind == "modem_message" and from and to then
    local hostFrom = string.match(from, "[^:]*")
    local hostTo = string.match(to, "[^:]*")
    if hostFrom == hostWired and hostTo == hostWireless then
      uuidWired = uuidFrom
      if uuidWireless then
        modemWireless.send(uuidWireless, channel, data, from, to)
      else
        modemWireless.broadcast(channel, data, from, to)
      end
    elseif hostFrom == hostWireless and hostTo == hostWired then
      uuidWireless = uuidFrom
      if uuidWired then
        modemWired.send(uuidWired, channel, data, from, to)
      else
        modemWired.broadcast(channel, data, from, to)
      end
    end
  end
end).run()
