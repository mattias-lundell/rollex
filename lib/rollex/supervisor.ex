defmodule Rollex.IRC.Supervisor do
  use Supervisor.Behaviour

  def start_link do
    :supervisor.start_link(__MODULE__, [])
  end

  def init([]) do
    children = [
      #worker(Rollex.IRC, [spid, "ssl://donger.alphajanne.com:6697"])
      # Define workers and child supervisors to be supervised
      # worker(Rollex.Worker, [])
    ]

    # See http://elixir-lang.org/docs/stable/Supervisor.Behaviour.html
    # for other strategies and supported options
    supervise(children, strategy: :one_for_one)
  end
end
