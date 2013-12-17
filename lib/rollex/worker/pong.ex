defmodule Rollex.Worker.Pong do
  use GenServer.Behaviour

  def start_link(args) do
    :gen_server.start_link(__MODULE__, args, [])
  end

  def init(_args) do
    IO.puts "Pong worker started :D"
    {:ok, []}
  end

  def handle_cast({Rollex.IRC.Command[type: :ping] = cmd, istate}, _s) do
    IO.puts "Pong worker received ping from: " <> Enum.join(cmd.params, " ")
    Rollex.IRC.pong(istate.server, cmd.params) 
    {:noreply, _s}
  end
end
