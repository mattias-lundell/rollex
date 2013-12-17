defmodule RollexTest do
  use ExUnit.Case

  setup_all do
    Socket.Manager.start(nil, nil)
    :ssl.start()
  end

  test "should connect to an SSL IRC server" do
    server = "ssl://chat.freenode.net:6697"
    {reply, _s} = Rollex.IRC.connect(server)
    assert reply == :ok
  end

  # Learning test
  #test "should receive strings from server" do
  #  server = "ssl://chat.freenode.net:6697"
  #  {:ok, state} = Rollex.IRC.connect(server)
  #  {reply, _m, lines} = Rollex.IRC.process(state, [])
  #  IO.puts lines
  #  assert reply == :error
  #end

  # Learning test for elixir-socket
  #test "should connect to an SSL IRC server socket" do
  #  server = "ssl://chat.freenode.net:6697"
  #  {reply, _sock} = Socket.connect(server)
  #  assert reply == :ok
  #end

  test "should parse inbound server message" do
    msg = ":drop.disrespect.com NOTICE Auth :*** Looking up your hostname"
    cmd = msg |> Rollex.IRC.inbound

    assert cmd.user.name == "drop.disrespect.com"
    assert cmd.type == :notice
    assert cmd.params == String.split("Auth :*** Looking up your hostname")
  end


  test "should create string for sending" do
    type = "PONG"
    from = ":some.server.com"
    cmd = type |> Rollex.IRC.command_type |>
      Rollex.IRC.irc_command([from])

    assert cmd |> Rollex.IRC.outbound == type <> " " <> from <> "\r\n"
  end

  test "should parse inbound raw message to record" do
    raw = "PING :server.host.com"
    cmd = Rollex.IRC.inbound(raw)

    assert is_record(cmd)
    assert cmd.type == :ping
    assert cmd.user != nil
    assert cmd.user.name != nil
    assert cmd.user.nick != nil
    assert cmd.params == [":server.host.com"]
  end

  test "should create irc command of requested type" do
    params = [":hello.there"]
    cmd = Rollex.IRC.irc_command(:pong, params)

    assert is_record(cmd)
    assert cmd.type == :pong
    assert cmd.params == params
  end

  test "should get user from raw user string" do
    raw_user = ":sherlockj!~sherlock_@host.isp.yeah.com"
    user = Rollex.IRC.get_user(raw_user)

    assert is_record(user)
    assert user.name == "~sherlock_"
    assert user.nick == "sherlockj"
  end
end
