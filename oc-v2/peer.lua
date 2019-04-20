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
