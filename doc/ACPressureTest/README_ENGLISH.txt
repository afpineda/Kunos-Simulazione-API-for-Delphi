==================================
AUTOMATED PRESSURE TEST FOR AC/ACC
==================================

------------------------
WHAT IS A PRESSURE TEST?
------------------------

A pressure test will compute the correct cold tire pressure in order to achieve a target hot tire pressure after a few laps.
Such computed tire pressure will remain valid as long as:
- track/air temperature does not change. However, you may easily predict cold tire pressure for any temperature. See below.
- tire compound remains the same
- car setup and track remains the same

You should write down tire compound, track/air temps, car, track and car setup along with the correct cold pressures.

------------------------------------------------------------------
HOW DO I PREDICT COLD tire PRESSURE FOR ANY AIR/TRACK TEMPERATURE?
------------------------------------------------------------------

Apply this formula:

Let be DT = MAX (Current air temp - Test air temp, current track temp - test track temp)

Predicted cold pressure = test result - (0,1 * DT)

------------------------------------------------
HOW DO I (RE)START THE AUTOMATED PRESSURE TEST?
------------------------------------------------

Enter a practice session (hot lap modes are not suitable).
Go to the setup screen, then click on "drive" within a time lapse of 5 seconds.
Otherwise, cold pressures will not be detected.

-----------------------------------------
HOW DO I RUN THE AUTOMATED PRESSURE TEST?
-----------------------------------------

Just go out and drive some laps. Try not to touch any kerb.
For qualify setups:
- Run the test for 2 or 3 laps.
- Use low fuel load

For race setups:
- Run the test for no less than 5 laps
- Use full fuel load

Hot pressures are measured at the finish line, however you may choose another track point.
Just drive to that point and pause the game (or stop the car).
Then, click on "Options > Pressure test options > Read hot pressures at current car position". 

-----------------
WHAT IS THIS FOR?
-----------------

"Actions > Restart test":
Run another test from start

"Actions > Set target pressures to default":
Use predefined target pressures for current tire compound. Useful after the test is finished.

"Actions > Copy results to clipboard":
After the test is finished, copy results in table format, suitable for any spreadsheet app (press CTRL-V to paste).

"Actions > Read hot pressures at finish line":
Autodescriptive.

"Options > Pressure test options > Read hot pressures at current car position":
See above.

"Options > Pressure test options > Complete test at garage teleporting":
The pressure test will finish at the time you enter the pause menu, and choose "return to garage".
This way you may run any number of laps.

"Options > Pressure test options > Complete test at NN laps":
The pressure test will finish automatically after a predefined number of laps.

"Options > App options > Keep this window visible":
App window will remain on top of game screen.

"Options > App options > Copy-paste options":
Select which data fields (and order) to copy-paste. Useful in order to write down your test result into a spreadsheet.

"Options > App options > Clean my Window's registry key":
This app will store user options at the Windows' registry. Choose this to clean such options if you plan to stop using it.
Will be effective after the app is closed.







