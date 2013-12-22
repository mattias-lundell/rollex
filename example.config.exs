Rollex.Config.config do
  config.servers ["ssl://chat.freenode.net:6697"]
  config.channels ["#rollex"]
  config.workers [Rollex.IRC.Worker.new.module(Rollex.Worker.Pong).interests([:ping])]
  config.user Rollex.IRC.User.new.nick("rollex").name("rollex")
end
