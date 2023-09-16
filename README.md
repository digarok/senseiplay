# SenseiPlay

NTP "Module" player for Apple IIgs by digarok of NinjaForce

This uses the NinjaTrackerPlus playback engine developed by Jesse Blue of NinjaForce.

## About 
The code is written in 65816 Assembly Language (Merlin compatible).

```
└── src
    ├── p8tools.s         <- lib - Prodos8 tools
    ├── scrollist.s       <- lib - scroll list widget
    ├── sp.s              <- Main Program (Prodos8)
    ├── texttools.s       <- lib - text tools
    └── vubars.s          <- code to render text VUs
```

## Goals
Initially this was an internal tool to test directories full of converted songs, but it's expanded to include a full file navigation UI and playback UI.

The overarching goals for the way it is written are:
- Boot from Prodos8 so it can fit on a floppy with multiple songs for a musicdisk
- Use very little memory (again is a P8 app) to be able to even run on original 256KB machines
- Have a unique style, even in text mode.  

## Checkout and Build
After you clone this repository you will need to add the ninjatrackerplus sources to your the ninjatrackerplus directory.  
https://www.ninjaforce.com/downloads/ntpsources.zip

#### Internal only instructions for NFC team members
```
git submodule init  
git submodule update

#git pull origin master
#git submodule update --remote

appy brun
```

You can build it using my project tool which will also generate a disk image, (Appy)[https://github.com/digarok/appy], or just run merlin32 on the source yourself. 
```
merlin32 -V . src/sp.s
``` 