   %% 
% Pilot Runabout ARENA experiment 
%
% Copyright 2014 Hedderik van Rijn
%
% v22: Changed by Andrea 
% 
%%
    
clear java
clear all
close all 

global par;

InitializePsychSound(1);
pahandle = PsychPortAudio('Open', [], [], 2,44100, 1, [], 0.015);

%% Info about the experiment:
par.expName = 'Run';
par.expVersion = '1'; 
par.expChangeData = '140212';

%% Keyboard settings:
KbName('UnifyKeyNames');

%% Debugging stuff:
% Enter the debugger when an error/warning is signalled.
dbstop if error
dbstop if warning

%% Define variables of the different experimental parts. 

    par.ExploreDur = 120; % I would recommend leaving this at a reasonabl high
    par.PoleDur = 55;
    par.StimulusDur = 55;
    par.numExpTrials= 30 ;  % Needs to be an even number
       
    

%% Set half of the trials to condition 1, the other half to condition 2.
expTrials(1:(par.numExpTrials/2))= 1;
expTrials(1+(par.numExpTrials/2):par.numExpTrials) = 3;

rep = 5;
% Make sure that there are never 3 trials of the same condition following
% eachother: 
while (rep > 3) 
    expTrials = Shuffle(expTrials);
    rep=max(diff(find(diff([-Inf expTrials Inf]))));
end

%% Start Arena

 arena = LoB;

if (ispc) 
    par.port_nat = 'COM5';
    par.port_prog = 'COM4';
end

if (ismac) 
    par.port_nat = '/dev/tty.usbmodem2621';
    par.port_prog = '/dev/tty.usbmodem411';
end

if (IsLinux)
    % Assume there is just one ACM decive attached:
    sPorts = instrhwinfo('serial');
    sPorts = sPorts.SerialPorts;
    par.port = sPorts(strncmp('/dev/ttyACM',sPorts,11));
end


arena.Init(par.port_nat,par.port_prog)
arena.Message('Init done');

arena.LED(0,0);

arena.SetBaseTemp(15);
arena.SetTileTemp(22,22,22);

arena.Wait('Init...',5); 

startTime = datestr(now,'yymmddHHMM');

GetChar; 

arena.Wait('Explore...',par.ExploreDur);

    expOnsetTime = GetSecs();
    curTrial = 0;
    curSafeTile = 1;
 
   %% Start the experimental block
   
%    arena.StartSampling(sprintf('TempReg_%s.dat',datestr(now)));
   
   trial_time=[];
   tic
   while (curTrial < par.numExpTrials)

        % Pole position
         
         curTrial = curTrial + 1;
         arena.SetTileTemp(34,22,34)
         arena.Wait(sprintf('%dPP',curTrial),par.PoleDur);
         arena.SetTileTemp(22,22,22)
         pause(5);
        
        % Test Phase

        trial_time(curTrial)=toc;
        curSafeTile = expTrials(curTrial);
        
        
        if (curSafeTile == 1)
            [sounddata freq] = wavread('sineShort.wav');     % Sounds are 44100, 400Hz, .3/.9 long sines
            PsychPortAudio('FillBuffer', pahandle, sounddata');    
            arena.SetTileTemp(40,40,22);
            PsychPortAudio('Start', pahandle);
            arena.LED(0,1);

            %arena.Boost(0,1,1)
                      
        end
        if (curSafeTile == 3)
            [sounddata freq] = wavread('sineLong.wav');
            PsychPortAudio('FillBuffer', pahandle, sounddata');
            arena.SetTileTemp(22,40,40);
            PsychPortAudio('Start', pahandle);
            arena.LED(1,0);

            %arena.Boost(1,1,0);
              
        end
        
        arena.Wait(sprintf('%dStim',curTrial),par.StimulusDur);
        arena.SetTileTemp(22,22,22)
        pause(5);
        arena.LED(0,0);
        safetile(curTrial)=curSafeTile;
        arena.Flush();
   end
  
   arena.SetTileTemp(22,22,22);
   
%    arena.Wait('Stopping',3);
%    arena.StopSampling();
   
   FileName=sprintf('TimeSafe_%s.csv',datestr(now));
   timesafe=[trial_time; safetile];
   csvwrite(FileName,timesafe);
   PsychPortAudio('Close');     % Necessary to close audio channel

  
 