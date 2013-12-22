defmodule Rollex.Config do
  use ExConfig.Object
  defproperty servers, default: ["ssl://chat.freenode.net:6697"]
  defproperty channels, default: ["#rollex"]
  defproperty workers, default: [Rollex.IRC.Worker.new.module(Rollex.Worker.Pong).interests([:ping])]
  defproperty user, default: Rollex.IRC.User.new.nick("rollex").name("rollex").realname("I r rollex bot")
end
