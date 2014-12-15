This was my first real haskell project. It's a 2d space game, using GTK3.

Long term goals is for this to be a polished game that's enjoyable to play, 
and possibly to migrate the game over to use FRP.

Here are some screenshots:

![Space launches missiles, "x" changes missile type](http://i.imgur.com/H4v2YMy.png)


![Explosive missiles have larger explosion radiuses](http://i.imgur.com/bEPXTJK.png)


![Seeking missiles seek the ship targeted with TAB](http://i.imgur.com/sQRZ3lR.png)

To build/install this game, just download a branch and then type:
cabal configure
cabal build
cabal install

Then just run the OpenSkies executable located in your .cabal folder.
(it was compiled with the -threaded flag, so feel free to run it
with +RST -N to run with multiple processesors) 

The player located at the center of the screen.
Left and right turn the ship, and foward moves the ship forward. The ship
stays centered in the screen, but the world moves around it.

"x" changes the weapon selected, and space fires it.
For the "SeekingMissle" weapon, pressing tab cycles through the visible enemies,
and firing a seeking missile when an enemy is selected will
cause it to seek out the enemy and destroy it.

Currently a game-condition (or pausing or something when the player health
reaches zero) is not implemented.
