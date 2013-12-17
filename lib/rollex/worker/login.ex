defmodule Rollex.Worker.Login do
  use GenServer.Behaviour

  def start_link([]) do
    :gen_server.start_link(__MODULE__, [], [])
  end

  def init([]) do
    {:ok, []}
  end

  def handle_cast({Rollex.IRC.Command[type: :notice] = cmd, istate}, _s) do
    IO.puts "Pong worker received ping from: " <> Enum.join(cmd.params, " ")
    Rollex.IRC.pong(istate.server, cmd.params) 
    {:noreply, _s}
  end
end
