
n=NetServiceClock.new.start

c=NetClientClock("emacs", "127.0.0.1", "127.0.0.1");
c.connect;
c.sync(1)

c.tempo_(120/60, 0)

