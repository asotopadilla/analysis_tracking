%% ----- Code by Andrea Soto Padilla ----- %%
% ------ Version: 17.0 ------ %
% ------ Date: 4 March 2017 ------ %
% ------ Modified by Andrea Soto ----- %
% Modifications:
% V2. Fixed how it determined correct or incorrect.
% V3. Changed error .csv to detect every file non equal to numphases, not
% only those with less than numphases.
% V4. For LED type 3, if no left or right LEDs are found code does not
% return erro but saves a file .csv saying it does not have enough phases.
% After graph saved workspace variables to disk, clear memory, and reload
% workspace variables to prevent java memory error (which stops process).
% V5. Changed way code stops if there is a problem with a video so it works
% with all versions of matlab.
% Added standard deviation to strip plots
% V6. For videos that have inverted pole, also inverted LEDs.
% V7. Create matrix to input in R (all flies and variables) and create
% check-up for dead flies that allows to determine what to do if they are
% detected (continue, not continue, use values, set to NaN).
% V8. Added that if the fly starts in the Unsafe the trial is also considered invalid
% - no time to safe - and modified track graphs to include when pole was
% incorrect.
% V9. Added columns of going to close, going to previously safe, and mode probes (based on numbins
% (e.g numbins=3 then 1=first 20seconds, 2=second 20 seconds, 3=last 20
% seconds) to table for Hedderik; created PP table; created graph of when
% probes happened.
% V10.Added perspective correction based on 10 more pixels at the bottom of the left and 16 more pixles
% at the bottom of the right side (our videos have less because the tracker gets wrong points.
% V11. Added a correction of perspective to put the lines separating tiles
% in the correct place.
% V12. Added the option to set some variables to NaN if fly starts outside
% of pole. Some plots were commented out by % % %
% V13. Added separate phase duration for each trial and poles, and changed
% how number of frames for dead fly definition based on new durations
% V14. Added HeatBox condition: it will force the safe location to be the
% same as the pole tile.
% V16. Added LED5 and changed how minutes are calculated (before was based
% on all trials with identical frames and now it allows phases with
% different durations). 
% V17. Converted speeds to cm/s, time to secs and distance to cm

%% ------Output Description------
%Columns - Description _perflydata.csv
%[01] - X position
%[02] - Y position]
%[03] - Mean speed in cm/s
%[0e] - Minute

%Columns - Description _perflyborders.csv -- top, bottom, left, right
%[01] - Minute (only one at the beginning)
%[02] - Seconds inside border
%[03] - cms occupied the border
%[04] - Proportio of area used

%Columns - Description _perfly_bouts.csv
%[01] - Minute
%[02] - Frame
%[03] - Speed in cm/s
%[04] - Duration in seconds

%Columns - Description _perfly_stops.csv
%[01] - Minute
%[02] - Frame
%[03] - Speed in cm/s
%[04] - Duration in seconds

%% Clear memory and worspace
clear all
clear java
clc

%% -------Define Variables-------

% Input files
conditions={'CK_curve36_'}; %Name for conditions (with underscore)
flies={'f_', 'm_'}; %name for flies (with underscore)
filesuffix='_output'; %end name of file

% Experimental set up
numvideos=1:3; %number of videos in experiment for each condition
numphases=28; %number of phases (phase = pole and trial)
numflies=1; %number of flies to be tracked
ledtype=5; %set LED type [1 - Blink with PP lights, 2 - Blink no PP lights, 3 - Constant no PP lights, 4 - Constant with PP lights, 5 - Constant left or right used to mark time.]
poles='No'; %Set 'Yes' or 'No' for whether the experiment has pole position or not
poleduration=0; %duration of trial in seconds. Irrelevant for LED type 5.
lefttrialduration=60; %duration of trial in seconds. Irrelevant for LED type 5.
righttrialduration=60; %duration of trial in seconds. Irrelevant for LED type 5.
timebeforestart=0; %duration in seconds of pre experimental time
border=30; %width in pixels of border to check for fly presence
numtiles=3; %Number of tiles used in experiment

% Video info
arenax=[281 1058]; %left and right edges of video
arenay=[343 586]; %top and bottom edges of video
fps=30; %frames per second of video
pixelcmratio=0.00899; %cm/pixel ratio
invertvid=0; %Set video range (i.e. 1:10 or 11:20) to have x coordinates inverted (left to right). Set to 0 for no inversion

% Arena Info
fixperspective=0; %Set to (1) to correct perspctive in x-y coordinates or (0) not to
leftdiffperspective=10; %Set x-coordinate difference in pixels between perspective left top edge and flat left top edge
rightdiffperspective=16; %Set x-coordinate difference in pixels between perspective right top edge and flat right top edge

% Output Settings
stopifdeadflies=0; %Set to 1 to stop execution if dead flies are detected or 0 to continue

% Threshholds
secstop=1; %number of seconds to consider the fly as stopped.
secsbout=1; %number of seconds to consider the fly as bout.
minspeed=1.5; %minimum speed (px/frame) to consider fly as bout
maxspeed=0.5; %maximum speed (px/frame) to consider fly as stopped
speedlimit=12; %top speed fly can move (cm/sec)
%% ------------------------------

%Set columns for x and y position data for multiple flies
xcolumn=4:2:14;
ycolumn=xcolumn+1;

%Find tile edges
tile_l=diff(arenax)/3+rightdiffperspective;
tile_r=2*(diff(arenax)/3)+rightdiffperspective;

%Define arena border
border_t=diff(arenay)-border;
area_t=diff(arenax)*border;
border_b=border;
area_b=diff(arenax)*border;
border_l=border;
area_l=diff(arenay)*border;
if numtiles==3
border_r=diff(arenax)-border;
else
border_r=tile_r-border;
end
area_r=diff(arenay)*border;


%Get transdormation matrix to correct perspective
movingPoints=[0 0; leftdiffperspective diff(arenay); diff(arenax)-rightdiffperspective diff(arenay); diff(arenax) 0];
fixedPoints=[0 0; 0 diff(arenay); diff(arenax) diff(arenay); diff(arenax) 0];
tranform=fitgeotrans(movingPoints,fixedPoints,'projective');

%Combine conditions and flies to form file name root
k=1;
for temp_i=1:length(conditions)
    for j=1:length(flies)
        root(k)=strcat(conditions(temp_i), flies(j));
        k=k+1;
    end
end
clearvars i j k conditions flies

%% Checks that all videos exist, can be loaded or have the correct number of phases otherwise stop
stop=0;
for condition=1:length(root)
    for video=numvideos
        
        fprintf('Checking video %s out of %s for condition %s.\n',int2str(video),int2str(max(numvideos)),strrep(root{condition},'_',''));
        
        %Set file name
        filename=[root{condition} int2str(video) filesuffix '.csv'];
        
        %If file does not exist, outputs a file with a warning
        if ~exist(filename,'file')
            csvwrite([root{condition} int2str(video) filesuffix '_file_does_not_exist.csv'],[]);
            stop=1;
            pause(0.25);
            clc
            continue
        end
        try
            %Load data
            Data=csvread(filename,2,0);
        catch
            %If file cannot be red, outputs a file with a warning
            csvwrite([root{condition} int2str(video) filesuffix '_cannot_read_file.csv'],[]);
            stop=1;
            pause(0.25);
            clc
            continue
        end
        
        %Project points from perspective arena onto flat plane
        if fixperspective==1
            Data(:,4:5)=transformPointsForward(tranform,Data(:,4:5));
            Data(Data(:,4)>diff(arenax),4)=diff(arenax);
            Data(Data(:,4)<0,4)=0;
        end
        
        %Determine phases from LEDs based on LED Type set above
        %Find where the LEDs are on
        led_left=find(Data(:,2)==1);
        led_right=find(Data(:,3)==1);
        
        %Determine frames for poles and trials
        if ledtype==1 || ledtype==2 %led type 1 and 2
            
            %Find how long leds are on for
            led_left_length=diff(led_left);
            led_left_length=find([led_left_length; inf]>1);
            led_left_length=diff([0; led_left_length]);
            %Find the last frame where the led is on
            led_left=led_left(cumsum(led_left_length));
            %If led in on for less than 5 frames or more than 200, disconsider it
            for led_left_counter=1:length(led_left)
                if led_left_length(led_left_counter)<5 || led_left_length(led_left_counter)>200
                    led_left(led_left_counter)=NaN;
                end
            end
            led_left(isnan(led_left))=[];
            
            %Find how long leds are on for
            led_right_length=diff(led_right);
            led_right_length=find([led_right_length; inf]>1);
            led_right_length=diff([0; led_right_length]);
            %Find the last frame where the led is on
            led_right=led_right(cumsum(led_right_length));
            %If led in on for less than 5 frames or more than 200, disconsider it
            for led_right_counter=1:length(led_right)
                if led_right_length(led_right_counter)<5 || led_right_length(led_right_counter)>200
                    led_right(led_right_counter)=NaN;
                end
            end
            led_right(isnan(led_right))=[];
            
            %Corrects for when left and right leds turn off 1 frame appart on pole
            for led_left_counter=2:length(led_left)
                if ismember(led_left(led_left_counter)+1,led_right)
                    led_left(led_left_counter)=led_left(led_left_counter)+1;
                elseif ismember(led_left(led_left_counter)-1,led_right)
                    led_left(led_left_counter)=led_left(led_left_counter)-1;
                end
            end
            
            if ledtype==1
                %Find the frames where pole and trials start
                pole=intersect(led_left,led_right);
                left=setdiff(led_left,pole);
                right=setdiff(led_right,pole);
            else
                %Find the frames where pole and trials start
                left=led_left;
                right=led_right;
                
                if strcmp(poles,'Yes')
                    pole=sort([left; right]-poleduration*fps);
                else
                    pole=[];
                end
            end
            
        elseif ledtype==3 %LED type 3
            
            %Find where left LEDs are turned on
            led_left=find(Data(:,2)==1);
            if ~isempty(led_left)
                %Find how long LEDs are on for
                led_left_length=diff(led_left);
                led_left_begin=find([inf; led_left_length;]>100);
                led_left_end=find([led_left_length; inf]>100);
                left=led_left(led_left_begin);
            else
                left=[];
            end
            
            %Find where right LEDs are turned on
            led_right=find(Data(:,3)==1);
            if ~isempty(led_right)
                %Find how long LEDs are on for
                led_right_length=diff(led_right);
                led_right_begin=find([inf; led_right_length]>100);
                led_right_end=find([led_right_length; inf]>100);
                right=led_right(led_right_begin);
            else
                right=[];
            end
            
            %Find the poles
            if (~isempty(led_left) || ~isempty(led_right)) && strcmp(poles, 'Yes')
                pole=sort([1; led_left(led_left_end)+1; led_right(led_right_end)+1]);
                pole=pole(1:end-1);
            else
                pole=[];
            end
            
        elseif ledtype==4 %LED type 4
            
            %Find where left LEDs are turned on
            led_left=find(Data(:,2)==1);
            %Find where right LEDs are turned on
            led_right=find(Data(:,3)==1);
            %Find where both are turned on
            led_pole=led_left(ismember(led_left, led_right));
            %Remove when both LEDs are on from left and right LEDs
            led_left=led_left(~ismember(led_left, led_pole));
            led_right=led_right(~ismember(led_right, led_pole));
            
            if ~isempty(led_left)
                %Find how long LEDs are on for
                led_left_length=diff(led_left);
                led_left_begin=find([inf; led_left_length;]>100);
                led_left_end=find([led_left_length; inf]>100);
                left=led_left(led_left_begin);
            else
                left=[];
            end
            
            
            if ~isempty(led_right)
                %Find how long LEDs are on for
                led_right_length=diff(led_right);
                led_right_begin=find([inf; led_right_length]>100);
                led_right_end=find([led_right_length; inf]>100);
                right=led_right(led_right_begin);
            else
                right=[];
            end
            
            if ~isempty(led_pole)
                %Find how long LEDs are on for
                led_pole_length=diff(led_pole);
                led_pole_begin=find([inf; led_pole_length]>100);
                led_pole_end=find([led_pole_length; inf]>100);
                pole=led_pole(led_pole_begin);
            else
                pole=[];
            end
            
        elseif ledtype==5
            
            %Find where left LEDs are turned on
            led_left=find(Data(:,2)==1);
            %Find where right LEDs are turned on
            led_right=find(Data(:,3)==1);
            %Find where both are turned on
            led_pole=led_left(ismember(led_left, led_right));
            %Remove when both LEDs are on from left and right LEDs
            led_left=led_left(~ismember(led_left, led_pole));
            led_right=led_right(~ismember(led_right, led_pole));
            
            if ~isempty(led_left)
                %Find how long LEDs are on for
                led_left_length=diff(led_left);
                led_left_begin=find([inf; led_left_length;]>100);
                led_left_end=find([led_left_length; inf]>100);
                left=led_left(led_left_begin);
            else
                left=[];
            end
            
            if ~isempty(led_right)
                %Find how long LEDs are on for
                led_right_length=diff(led_right);
                led_right_begin=find([inf; led_right_length]>100);
                led_right_end=find([led_right_length; inf]>100);
                right=led_right(led_right_begin);
            else
                right=[];
            end
            
            pole=[];
            
        end
        
        %make one vector with all the poles and trials in order (phases - pole=0, left=1, right=2)
        phases=[pole; left; right];
        phases(ismember(phases,pole),2)=0;
        phases(ismember(phases,left),2)=1;
        phases(ismember(phases,right),2)=2;
        
        if ledtype==5
            phases(:,3)=[led_left_end-led_left_begin; led_right_end-led_right_begin];
        else
            phases(phases(:,2)==0,3)=poleduration*fps;
            phases(phases(:,2)==1,3)=lefttrialduration*fps;
            phases(phases(:,2)==2,3)=righttrialduration*fps;
        end
        
        phases=sortrows(phases, 1);
        
        if size(phases, 1)~=numphases
            csvwrite([root{condition} int2str(video) filesuffix '_only_' int2str(size(phases, 1)) '_phases.csv'],[]);
            stop=1;
            pause(0.25);
            clc
        end
        
        if phases(1,1)==0
            phases(1,1)=1;
        end
        
        %Check for dead flies
%         if length(Data)>=phases(end,1)+1+phases(end,3)
%             x_time=phases(end,1)+1+phases(end,3);
%         else
%             x_time=length(Data);
%         end
%         
%         x_left=diff([0; find([diff(Data(Data(1:x_time,xcolumn(1:numflies))<=tile_l,1)); inf]>1)]);
%         x_middle=diff([0; find([diff(Data(Data(1:x_time,xcolumn(1:numflies))>tile_l & Data(1:x_time,xcolumn(1:numflies))<tile_r,1)); inf]>1)]);
%         x_right=diff([0; find([diff(Data(Data(1:x_time,xcolumn(1:numflies))>=tile_r,1)); inf]>1)]);
%         x_max=max([x_left; x_middle; x_right]);
%         
%         if x_max>(poleduration+lefttrialduration+righttrialduration)*fps
%             csvwrite([root{condition} int2str(video) filesuffix '_fly_died.csv'],[]);
%             if stopifdeadflies==1
%                 stop=1;
%             end
%             pause(0.25);
%             clc
%         end
        
        clearvars led_* left right pole x_*
        
        clc
    end
end

clearvars condition video phases temp*

%Stop execution if a problem was found
if stop==1
    clc
    clear all
    error('A problem was found with one of the files. Please check CSVs for more details.');
end

%% Start Analysis

for condition=1:length(root)
    
    Data_all=[];
    Data_border=[];
    xys=[];
    
    for video=numvideos
        
        %Set file name
        filename=[root{condition} int2str(video) filesuffix '.csv'];
        
        %If file does not exist, outputs a file with a warning
        if ~exist(filename,'file')
            csvwrite([root{condition} int2str(video) filesuffix '_file_does_not_exist.csv'],[]);
            continue
        end
        try
            %Load data
            Data=csvread(filename,2,0);
        catch
            %If file cannot be red, outputs a file with a warning
            csvwrite([root{condition} int2str(video) filesuffix '_cannot_read_file.csv'],[]);
            continue
        end
        
        %Invert x values and leds for videos specified above
        if max(invertvid)~=0 && ismember(video, invertvid)
            Data(:,xcolumn(1:numflies))=diff(arenax)-Data(:,xcolumn(1:numflies));
            l=Data(:,2);
            r=Data(:,3);
            Data(:,2)=r;
            Data(:,3)=l;
            clearvars l r
        end
        
        %Project points from perspective arena onto flat plane
        if fixperspective==1
            Data(:,4:5)=transformPointsForward(tranform,Data(:,4:5));
            Data(Data(:,4)>diff(arenax),4)=diff(arenax);
            Data(Data(:,4)<0,4)=0;
        end
        
        %% Determine phases from LEDs based on LED Type set above
        
        %Find where the LEDs are on
        led_left=find(Data(:,2)==1);
        led_right=find(Data(:,3)==1);
        
        %Determine frames for poles and trials
        if ledtype==1 || ledtype==2 %led type 1 and 2
            
            %Find how long leds are on for
            led_left_length=diff(led_left);
            led_left_length=find([led_left_length; inf]>1);
            led_left_length=diff([0; led_left_length]);
            %Find the last frame where the led is on
            led_left=led_left(cumsum(led_left_length));
            %If led in on for less than 5 frames or more than 200, disconsider it
            for led_left_counter=1:length(led_left)
                if led_left_length(led_left_counter)<5 || led_left_length(led_left_counter)>200
                    led_left(led_left_counter)=NaN;
                end
            end
            led_left(isnan(led_left))=[];
            
            %Find how long leds are on for
            led_right_length=diff(led_right);
            led_right_length=find([led_right_length; inf]>1);
            led_right_length=diff([0; led_right_length]);
            %Find the last frame where the led is on
            led_right=led_right(cumsum(led_right_length));
            %If led in on for less than 5 frames or more than 200, disconsider it
            for led_right_counter=1:length(led_right)
                if led_right_length(led_right_counter)<5 || led_right_length(led_right_counter)>200
                    led_right(led_right_counter)=NaN;
                end
            end
            led_right(isnan(led_right))=[];
            
            %Corrects for when left and right leds turn off 1 frame appart on pole
            for led_left_counter=2:length(led_left)
                if ismember(led_left(led_left_counter)+1,led_right)
                    led_left(led_left_counter)=led_left(led_left_counter)+1;
                elseif ismember(led_left(led_left_counter)-1,led_right)
                    led_left(led_left_counter)=led_left(led_left_counter)-1;
                end
            end
            
            if ledtype==1
                %Find the frames where pole and trials start
                pole=intersect(led_left,led_right);
                left=setdiff(led_left,pole);
                right=setdiff(led_right,pole);
            else
                %Find the frames where pole and trials start
                left=led_left;
                right=led_right;
                
                if strcmp(poles,'Yes')
                    pole=sort([left; right]-poleduration*fps);
                else
                    pole=[];
                end
            end
            
        elseif ledtype==3 %LED type 3
            
            %Find where left LEDs are turned on
            led_left=find(Data(:,2)==1);
            if ~isempty(led_left)
                %Find how long LEDs are on for
                led_left_length=diff(led_left);
                led_left_begin=find([inf; led_left_length;]>100);
                led_left_end=find([led_left_length; inf]>100);
                left=led_left(led_left_begin);
            else
                left=[];
            end
            
            %Find where right LEDs are turned on
            led_right=find(Data(:,3)==1);
            if ~isempty(led_right)
                %Find how long LEDs are on for
                led_right_length=diff(led_right);
                led_right_begin=find([inf; led_right_length]>100);
                led_right_end=find([led_right_length; inf]>100);
                right=led_right(led_right_begin);
            else
                right=[];
            end
            
            %Find the poles
            if (~isempty(led_left) || ~isempty(led_right)) && strcmp(poles, 'Yes')
                pole=sort([1; led_left(led_left_end)+1; led_right(led_right_end)+1]);
                pole=pole(1:end-1);
            else
                pole=[];
            end
            
        elseif ledtype==4 %LED type 4
            
            %Find where left LEDs are turned on
            led_left=find(Data(:,2)==1);
            %Find where right LEDs are turned on
            led_right=find(Data(:,3)==1);
            %Find where both are turned on
            led_pole=led_left(ismember(led_left, led_right));
            %Remove when both LEDs are on from left and right LEDs
            led_left=led_left(~ismember(led_left, led_pole));
            led_right=led_right(~ismember(led_right, led_pole));
            
            if ~isempty(led_left)
                %Find how long LEDs are on for
                led_left_length=diff(led_left);
                led_left_begin=find([inf; led_left_length;]>100);
                led_left_end=find([led_left_length; inf]>100);
                left=led_left(led_left_begin);
            else
                left=[];
            end
            
            if ~isempty(led_right)
                %Find how long LEDs are on for
                led_right_length=diff(led_right);
                led_right_begin=find([inf; led_right_length]>100);
                led_right_end=find([led_right_length; inf]>100);
                right=led_right(led_right_begin);
            else
                right=[];
            end
            
            if ~isempty(led_pole)
                %Find how long LEDs are on for
                led_pole_length=diff(led_pole);
                led_pole_begin=find([inf; led_pole_length]>100);
                led_pole_end=find([led_pole_length; inf]>100);
                pole=led_pole(led_pole_begin);
            else
                pole=[];
            end
        
        elseif ledtype==5
            
             %Find where left LEDs are turned on
            led_left=find(Data(:,2)==1);
            %Find where right LEDs are turned on
            led_right=find(Data(:,3)==1);
            %Find where both are turned on
            led_pole=led_left(ismember(led_left, led_right));
            %Remove when both LEDs are on from left and right LEDs
            led_left=led_left(~ismember(led_left, led_pole));
            led_right=led_right(~ismember(led_right, led_pole));
            
            if ~isempty(led_left)
                %Find how long LEDs are on for
                led_left_length=diff(led_left);
                led_left_begin=find([inf; led_left_length;]>100);
                led_left_end=find([led_left_length; inf]>100);
                left=led_left(led_left_begin);
            else
                left=[];
            end
            
            
            if ~isempty(led_right)
                %Find how long LEDs are on for
                led_right_length=diff(led_right);
                led_right_begin=find([inf; led_right_length]>100);
                led_right_end=find([led_right_length; inf]>100);
                right=led_right(led_right_begin);
            else
                right=[];
            end
            
            pole=[];
                    
        end
        
        %make one vector with all the poles and trials in order (phases - pole=0, left=1, right=2)
        phases=[pole; left; right];
        phases(ismember(phases,pole),2)=0;
        phases(ismember(phases,left),2)=1;
        phases(ismember(phases,right),2)=2;   
        
        if ledtype==5
            phases(:,3)=[led_left_end-led_left_begin; led_right_end-led_right_begin];
        else
            phases(phases(:,2)==0,3)=poleduration*fps;
            phases(phases(:,2)==1,3)=lefttrialduration*fps;
            phases(phases(:,2)==2,3)=righttrialduration*fps;
        end
        
        phases=sortrows(phases, 1);
        
        if phases(2,1)>=phases(1,3)
            phases(1,1)=phases(2,1)-phases(1,3);
        end
        
        if phases(1,1)==0
            phases(1,1)=1;
        end
        
        clearvars led_* left right pole
        
        Data=Data(phases(1,1)-fps*timebeforestart:(phases(1,1)+sum(phases(:,3))),:);
 
        mins=[];
        for i=1:size(phases, 1)
            mins(length(mins)+1:length(mins)+phases(i, 3),1)=ones(1,phases(i, 3))*i;
        end
        
        Data(end,:)=[];
        
        if length(mins) > length(Data)
            mins=mins(1:length(Data));
        end
        
        Data(:,size(Data,2)+1)=mins;
        
        for fly=1:numflies
            
            %Calculate fly speed between frames for entire video
            speed = [0; hypot(diff(Data(:,xcolumn(fly))), diff(Data(:,ycolumn(fly))))]*fps*pixelcmratio;
%             speed(:,2) = [0; diff(speed)];
%             speed(speed(:,2)<speedlimit, 2)=NaN;
            %speed(speed(:,1)>=speedlimit, 1)=NaN;
            
            Datafly=[Data(:,xcolumn(fly)) Data(:,ycolumn(fly)) speed Data(:,size(Data,2))];
            
            %Find where and for how long fly slows down to minspeed for at least secstop
            temp_speed=[speed, speed, mins];
            temp_speed(temp_speed(:,1)<=maxspeed, 1)=0;
            temp_rels=find(diff([temp_speed(1, 1)-1, temp_speed(:, 1)']));
            temp_relspeed=[temp_speed(temp_rels, 1)'; diff([temp_rels, numel(temp_speed(:, 1)')+1])]';
            temp_relspeed(:,3)=[1; cumsum(temp_relspeed(1:end-1,2))+1];
            temp_relspeed(temp_relspeed(:,1)~=0 | temp_relspeed(:,2)<secstop*fps,:) = [];
            for i=1:size(temp_relspeed, 1)
            temp_speedstop(i, :)=[temp_speed(temp_relspeed(i,3), 3), temp_relspeed(i,3), nanmean(temp_speed(temp_relspeed(i,3):(temp_relspeed(i,3)+temp_relspeed(i,2)-1), 2))*fps*pixelcmratio, temp_relspeed(i,2)/fps];
            end
            clearvars temp_speed temp_rels* i
            
            %% Saves per fly per video results into csvs
            if exist('temp_speedstop','var')
                csvwrite([root{condition} int2str(video) '_' int2str(fly) '_perfly_stops.csv'], sortrows(temp_speedstop, 2));
            end
            
            %Find where and for how long fly speeds up to maxspeed for at least secsbout
            temp_speed=[speed, speed, mins];
            temp_speed(temp_speed(:,1)>=minspeed, 1)=9999;
            temp_speed(temp_speed(:,2)>=speedlimit, 2)=NaN;
            temp_rels=find(diff([temp_speed(1, 1)-1, temp_speed(:, 1)']));
            temp_relspeed=[temp_speed(temp_rels, 1)'; diff([temp_rels, numel(temp_speed(:, 1)')+1])]';
            temp_relspeed(:,3)=[1; cumsum(temp_relspeed(1:end-1,2))+1];
            temp_relspeed(temp_relspeed(:,1)~=9999 | temp_relspeed(:,2)<secsbout*fps,:) = [];
            for i=1:size(temp_relspeed, 1)
            temp_speedbout(i, :)=[temp_speed(temp_relspeed(i,3), 3), temp_relspeed(i,3), nanmean(temp_speed(temp_relspeed(i,3):(temp_relspeed(i,3)+temp_relspeed(i,2)-1), 2))*fps*pixelcmratio, temp_relspeed(i,2)/fps];
            end
            clearvars temp_speed temp_rels* i

            %% Saves per fly per video results into csvs
            if exist('temp_speedbout','var')
                csvwrite([root{condition} int2str(video) '_' int2str(fly) '_perfly_bouts.csv'], sortrows(temp_speedbout, 2));
            end
            
            %Find frame where fly was in border
            temp_t=Datafly(Datafly(:,2)>=border_t,:);
            temp_b=Datafly(Datafly(:,2)<=border_b,:);
            temp_l=Datafly(Datafly(:,1)<=border_l,:);
            temp_r=Datafly(Datafly(:,1)>=border_r,:);
            
            %Initialize matrix to save values
            temp_border_all(:,1)=[0; unique(mins(1:end-1))];
            
            %Find unique pixels and frames fly was in border and store it
            [temp_frms_t, temp_min_t]=hist(temp_t(:,4),unique(temp_t(:,4)));
            [temp_xy_t, ~, ~]=unique(temp_t(:,[1:2, 4]) , 'rows');
            [temp_pixels_t, ~]=hist(temp_xy_t(:,3),unique(temp_xy_t(:,3)));
            temp_min_t=temp_min_t(temp_frms_t>0);
            temp_pixels_t=temp_pixels_t(temp_frms_t>0);
            temp_frms_t=temp_frms_t(temp_frms_t>0);
            temp_border_all(temp_min_t+1,2:4)=[(temp_frms_t/fps)' (temp_pixels_t*pixelcmratio)' (temp_pixels_t/area_t)'];
            
            [temp_frms_b, temp_min_b]=hist(temp_b(:,4),unique(temp_b(:,4)));
            [temp_xy_b, ~, ~]=unique(temp_b(:,[1:2, 4]) , 'rows');
            [temp_pixels_b, ~]=hist(temp_xy_b(:,3),unique(temp_xy_b(:,3)));
            temp_min_b=temp_min_b(temp_frms_b>0);
            temp_pixels_b=temp_pixels_b(temp_frms_b>0);
            temp_frms_b=temp_frms_b(temp_frms_b>0);
            temp_border_all(temp_min_b+1,5:7)=[(temp_frms_b/fps)' (temp_pixels_b*pixelcmratio)' (temp_pixels_b/area_b)'];
            
            [temp_frms_l, temp_min_l]=hist(temp_l(:,4),unique(temp_l(:,4)));
            [temp_xy_l, ~, ~]=unique(temp_l(:,[1:2, 4]) , 'rows');
            [temp_pixels_l, ~]=hist(temp_xy_l(:,3),unique(temp_xy_l(:,3)));
            temp_min_l=temp_min_l(temp_frms_l>0);
            temp_pixels_l=temp_pixels_l(temp_frms_l>0);
            temp_frms_l=temp_frms_l(temp_frms_l>0);
            temp_border_all(temp_min_l+1,8:10)=[(temp_frms_l/fps)' (temp_pixels_l*pixelcmratio)' (temp_pixels_l/area_l)'];
            
            [temp_frms_r, temp_min_r]=hist(temp_r(:,4),unique(temp_r(:,4)));
            [temp_xy_r, ~, ~]=unique(temp_r(:,[1:2, 4]) , 'rows');
            [temp_pixels_r, ~]=hist(temp_xy_r(:,3),unique(temp_xy_r(:,3)));
            temp_min_r=temp_min_r(temp_frms_r>0);
            temp_pixels_r=temp_pixels_r(temp_frms_r>0);
            temp_frms_r=temp_frms_r(temp_frms_r>0);
            temp_border_all(temp_min_r+1,11:13)=[(temp_frms_r/fps)' (temp_pixels_r*pixelcmratio)' (temp_pixels_r/area_r)'];
           
            temp_border_all(1,:)=[];
            
            %% Saves per fly per video results into csvs
            if exist('Datafly','var')
                csvwrite([root{condition} int2str(video) '_' int2str(fly) '_perflydata.csv'], Datafly);
            end
            
            %% Saves per fly per video results into csvs
            if exist('temp_border_all','var')
                csvwrite([root{condition} int2str(video) '_' int2str(fly) '_perflyborders.csv'], temp_border_all);
            end
            
            Data_all=cat(1, Data_all ,Datafly);
            Data_border=cat(3, Data_border, temp_border_all);
            
            clearvars *fly temp*
            
        end %end fly
        
        clearvars *fly phases
        
    end %end video
        
    %Average border data for all flies
    Data_border=nanmean(Data_border, 3);
    
    %% Saves condition border results into csvs
    if exist('xys','var')
        csvwrite([root{condition} 'borders.csv'],Data_border);
    end
    
    %Remove velocities above 5 times mean speed and aggregate by X Y coordinate
    Data_all(Data_all(:,3)>=nanmean(Data_all(:,3))+2*nanstd(Data_all(:,3)),3)=NaN;
    [b, ~, n]=unique(Data_all(:,[1:2, 4]) , 'rows');
    xys2=accumarray(n , Data_all(:,3) , [] , @(x) nanmean(x));
    xys=cat(2 , b , xys2);
    xys=sortrows(xys,[3,1,2]);
    
    %% Saves condition speed results into csvs
    if exist('xys','var')
        csvwrite([root{condition} 'xysm.csv'],xys);
    end
    
    for i=0:max(Data_all(:,4))
        xysplot=xys(xys(:,3)==i,:);
        %Scatter plot of X-Y coordinates for all videos with speed as color
        plot([tile_l tile_l],[1 max(arenay)],'Color','r');
        hold on
        plot([tile_r tile_r],[1 max(arenay)],'Color','r');
        hold on
        scatter(xysplot(:,1),xysplot(:,2),1,xysplot(:,4))
        cmap=colormap(parula);
        cbar=colorbar;
        ylabel(cbar,'Speed (pixels/second)');
        title([strrep(root{condition},'_','') ' Speed ' num2str(i)]);
        xlabel('X Position (pixels)');
        xlim([0, diff(arenax)]);
        ylabel('Y Position (pixels)');
        ylim([0, diff(arenay)]);
        saveas(gcf, [strrep(root{condition},'_','') '_speed_' num2str(i)], 'png');
        close all
        
        %3D Scatter plot of X-Y coordinates for all videos with speed as color
%         plot([tile_l tile_l],[1 diff(arenay)],'Color','r');
%         hold on
%         plot([tile_r tile_r],[1 diff(arenay)],'Color','r');
%         hold on
%         scatter3(xysplot(:,1),xysplot(:,2),xysplot(:,4),2,xysplot(:,4))
%         cmap=colormap(parula);
%         cbar=colorbar;
%         view([0 30]);
%         ylabel(cbar,'Speed (pixels/second)');
%         title([strrep(root{condition},'_','') ' Speed ' num2str(i)]);
%         xlabel('X Position (pixels)');
%         xlim([0, diff(arenax)]);
%         ylabel('Y Position (pixels)');
%         ylim([0, diff(arenay)]);
%         saveas(gcf, [strrep(root{condition},'_','') '_speed3D_' num2str(i)], 'png');
%         close all
    end

    
    clearvars Data* video fly xys*
    
end % end condition

%Play sound to alert program has ended
load handel;
sound(y(2000:17500),Fs);
pause(2)
clear all