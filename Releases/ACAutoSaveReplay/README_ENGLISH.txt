========================
AUTO SAVE REPLAY FOR ACC
========================

------------------------
WHAT IS THIS FOR?
------------------------

ACC has an autosave replay feature. However, it will record a maximum session time of 1 hour and 15 minutes.
This App will save entire sessions (only race or qualify) in one or more files.
It works both as a driver or spectator. 
It has been developed in order to help race directors and marshals.

------------------------
WHAT IS REQUIRED?
------------------------

First:
This App MUST run at the same computer that runs ACC. 

Second:
The broadcasting feature MUST be enabled.
Edit or create "My Documents\Assetto Corsa Competizione\Config\broadcasting.json" (UTF-8 encoded).
For example:

{
    "updListenerPort": 9000,
    "connectionPassword": "asd",
    "commandPassword": ""
}

Command password is not required by this App.

Third:
A simple key press must be configured to save replays. 
Go to "Options > Controls > Control bindings > Advanced > Save replay".
Shift/ctrl/alt modifiers and a few other keys are not supported right now.
You will be notified in such a case.

------------------------
HOW DOES IT WORK?
------------------------

At launch, your game configuration will be automatically loaded. So this is a zero-config App.
In a race/qualify session, it will send to ACC the configured key press in order to force a replay save. 
The default key is "M".

This is done at regular intervals depending on ACC configuration:
"Options > General > Replay max length"

If "Options > General > Auto save replay" is disabled, last autosave will be handled by this App.

-------------------------------
WHERE ARE THOSE FILES SAVED?
-------------------------------

Found at "My Documents\Assetto Corsa Competizione\Replay\Saved".
If the last autosave is handled by ACC, it will be saved to "My Documents\Assetto Corsa Competizione\Replay\Temp".

------------------------
APP MENU 
------------------------

DISABLE
-------
When checked, no save key press will be sent to ACC. However, you will be notified of autosave events.
Disable is automatically checked if "Replay max length" is lesser than 10 minutes in order to prevent
too many saved files, but you may uncheck this option at your will.

Save replay now (for testing)
-----------------------------
Use this option to ensure that autosave works.
You should see an in-game message like "saving replay...".

Show session state
------------------
Will tell you what this app thinks that is going on in ACC.

Reload config files
-------------------

*IMPORTANT*
If you change options in-game, this app will not notice that, unless you return to the main menu first.
If that is not the case, you should use this option after any change in the replay configuration or the "save replay" key.
