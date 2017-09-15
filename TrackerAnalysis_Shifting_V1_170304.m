%% ----- Code by Andrea Soto Padilla ----- %%
% ------ Version: 1.0 ------ %
% ------ Date: 4 March 2016 ------ %
% ------ Modified by Andrea Soto ----- %
% Modifications:
% Allows selecting which videos to track (1:10 instead of 10).

%% ------Output Description------
%Columns for per video and condition averages are the same
%Columns - Description
%[01] - Phase number
%[02] - Safe tile [0 - All tiles, 1 - Left, 2 - Middle, 3 - Right]
%[03] - Time in seconds which fly started moving (to calculate time to start moving)
%[04] - Time in seconds fly spent in the safe tile 
%[05] - Time in seconds fly spent in previously safe tile before reaching the safe tile 
%[06] - Time in seconds fly spent in unsafe tile before reaching the safe tile 
%[07] - Average speed per phase (cm/second)
%[08] - Average speed in the safe tile per phase (cm/second)
%[09] - Average speed in previously safe tile before reaching the safe tile per phase (cm/second)
%[10] - Average speed in unsafe tile before reaching the safe tile per phase (cm/second)
%[11] - Total distance walked in phase (cm)
%[12] - Distance traveled to reach safe tile (cm)

%% Clear memory and worspace
clear all
clear java
clc

%% -------Define Variables-------

% Input files
conditions={'SH_GR_','SH_WT_'}; %Name for conditions (with underscore)
flies={'M_'}; %name for flies (with underscore)
filesuffix='_output'; %end name of file

% Experimental set up
numvideos=1:5; %number of videos in experiment for each condition
numphases=28; %number of phases (phase = pole and trial)
numflies=1; %number of flies to be tracked
tileorder=[1 2 3]; %set order of tiles [1 - left tile, 2 - middle tile, 3 - right tile]
% for experiments with long code in control matlab:[3 1 2 2 3 2 3 1 2 1 3 2 3 1 2 1 3 2 3 1 2]
numexplorationphases=7; %Number of phases where all tiles in the same temperature

% Video info
arenax=[281 1058]; %left and right edges of video
arenay=[343 586]; %top and bottom edges of video
fps=30; %frames per second of video
pixelcmratio=0.00899; %cm/pixel ratio
invertvid=0; %Set video range (i.e. 1:10 or 11:20) to have x coordinates inverted (left to right). Set to 0 for no inversion

% Arena Info
fixperspective=1; %Set to (1) to correct perspctive in x-y coordinates or (0) not to
leftdiffperspective=10; %Set x-coordinate difference in pixels between perspective left top edge and flat left top edge
rightdiffperspective=16; %Set x-coordinate difference in pixels between perspective right top edge and flat right top edge

% Threshholds
mindist=0; %minimum distance from edge fly must be in tile to be considered (in)correct
minmove=3; %minimum distance in pixels to condire the fly has moved
mintime=0; %minimum number of frames fly must be in tile to be considered (in)correct
secondstodead=360; %number of seconds of no movement to consider the fly dead

% Output Settings
stopifdeadflies=0; %Set to 1 to stop execution if dead flies are detected or 0 to continue
nullifydeadflies=0; %If analysis continues with dead flies, set results to Null (1) or keep results (0)
%% ------------------------------

%Set columns for x and y position data for multiple flies
xcolumn=4:2:14;
ycolumn=xcolumn+1;

%Find tile edges
tile_l=diff(arenax)/3+rightdiffperspective;
tile_r=2*(diff(arenax)/3)+rightdiffperspective;

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
        
        %make one vector with all the poles and trials in order (phases - pole=0, left=1, right=2)
        phases=[left; right];
        phases(:,3)=[led_left_end-led_left_begin; led_right_end-led_right_begin];
        phases=sortrows(phases, 1);
        phases(1:numexplorationphases,2)=0;
        phases(numexplorationphases+1:end,2)=repmat(tileorder, 1, (length(phases)-numexplorationphases)/length(tileorder)   );
        
        if phases(2,1)>=phases(1,3)
            phases(1,1)=phases(2,1)-phases(1,3);
        end
        
        if size(phases, 1)~=numphases
            csvwrite([root{condition} int2str(video) filesuffix '_only_' int2str(size(phases, 1)) '_phases.csv'],[]);
            stop=1;
            pause(0.25);
            clc
        end
        
        %Check for dead flies
        if length(Data)>=phases(end,1)+1+phases(end,3)
            x_time=phases(end,1)+1+phases(end,3);
        else
            x_time=length(Data);
        end
        
        x_left=diff([0; find([diff(Data(Data(1:x_time,xcolumn(1:numflies))<=tile_l,1)); inf]>1)]);
        x_middle=diff([0; find([diff(Data(Data(1:x_time,xcolumn(1:numflies))>tile_l & Data(1:x_time,xcolumn(1:numflies))<tile_r,1)); inf]>1)]);
        x_right=diff([0; find([diff(Data(Data(1:x_time,xcolumn(1:numflies))>=tile_r,1)); inf]>1)]);
        x_max=max([x_left; x_middle; x_right]);
        
        if x_max>(secondstodead)*fps
            csvwrite([root{condition} int2str(video) filesuffix '_fly_died.csv'],[]);
            if stopifdeadflies==1
                stop=1;
            end
            pause(0.25);
            clc
        end
        
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

%Initialize some variables
result_all=[];

for condition=1:length(root)
    
    %Initialize some variables
    eval(['result_' strrep(root{condition},'_','') '=[];']);
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
        
        %make one vector with all the poles and trials in order (phases - pole=0, left=1, right=2)
        phases=[left; right];
        phases(:,3)=[led_left_end-led_left_begin; led_right_end-led_right_begin];
        phases=sortrows(phases, 1);
        phases(1:numexplorationphases,2)=0;
        phases(numexplorationphases+1:end,2)=repmat(tileorder, 1, (length(phases)-numexplorationphases)/length(tileorder)   );
        
        if phases(2,1)>=phases(1,3)
            phases(1,1)=phases(2,1)-phases(1,3);
        end
        
        clearvars led_* left right pole
        
        %initialize some variables
        curr_phase=1;
        phase=zeros(size(phases, 1),12);
        probesfly=[];
        
        for fly=1:numflies
            %% Start per phase calculations (phase = Pole Position or Trial)
            for curr_phase=1:size(phases, 1)
                
                %Saves current phase
                phase(curr_phase,1)=curr_phase;
                
                %Saves which tile is safe for the current trial
                phase(curr_phase,2)=phases(curr_phase,2);
                
                %Loads the data only for the current phase
                if curr_phase<size(phases, 1)
                    temp_data=Data(phases(curr_phase):phases(curr_phase+1)-1,:);
                else
                    if size(Data(phases(curr_phase):end,:),1)>=phases(curr_phase,3)
                        temp_data=Data(phases(curr_phase):phases(curr_phase)+1+phases(curr_phase,3),:);
                    else
                        temp_data=Data(phases(curr_phase)+1:end,:);
                    end
                end
                
                %Set the left and right edges for the safe tile according to current phase and which tile is the pole position
                if phases(curr_phase,2)==1 %left tile
                    temp_l_edge=0;
                    temp_r_edge=tile_l;
                elseif phases(curr_phase,2)==2 %middle tile
                    temp_l_edge=tile_l;
                    temp_r_edge=tile_r;
                elseif phases(curr_phase,2)==3 %right tile
                    temp_l_edge=tile_r;
                    temp_r_edge=diff(arenax);
                elseif phases(curr_phase,2)==0 %all tiles
                    temp_l_edge=0;
                    temp_r_edge=diff(arenax);
                end
                
                %Shift tile edges in by mindist
                temp_l_edge=temp_l_edge+mindist;
                temp_r_edge=temp_r_edge-mindist;
                
                %Initialize previously safe tile on first trial
                if curr_phase==1
                    temp_l_edgeprev=temp_l_edge;
                    temp_r_edgeprev=temp_r_edge;
                end
                
                %Check closest tile to start position (1 - left, 2 - middle, 3 - right)
                temp_startx=temp_data(1,xcolumn(fly));
                if temp_startx>=tile_l && temp_startx<diff(arenax)/2
                    temp_closesttile=0;
                elseif temp_startx<=tile_r && temp_startx>diff(arenax)/2
                    temp_closesttile=3;
                else
                    temp_closesttile=2;
                end
                
                %Find when fly first moves by at least minmove(set above)
                temp_i=1;
                temp_dist=0;
                while temp_dist<minmove && temp_i<length(temp_data)
                    temp_dist=pdist([temp_data(1,xcolumn(fly):ycolumn(fly)); temp_data(temp_i,xcolumn(fly):ycolumn(fly))],'euclidean');
                    temp_i=temp_i+1;
                end
                
                %Save which frame the fly first moved
                if temp_i<length(temp_data)
                    phase(curr_phase,3)=temp_i/fps;
                else
                    phase(curr_phase,3)=NaN;
                end
                
                %Find all frames when fly is inside the safe tile for at
                %least mintime, how long they were there for and how much
                %they walked.
                %Columns:
                %(1) Frame entered correct tile
                %(2) Number of frames until leaving safe tile again
                %(3) Distance walked before leaving safe tile
                temp_correct=find((temp_data(:,xcolumn(fly))>=temp_l_edge & temp_data(:,xcolumn(fly))<=temp_r_edge)==1);
                if ~isempty(temp_correct)
                    temp_correct_length=diff(temp_correct);
                    temp_correct_length=find([temp_correct_length; inf]>1);
                    temp_correct_length=diff([0; temp_correct_length]);
                    temp_correct_index=cumsum(temp_correct_length);
                    temp_correct=[temp_correct([1; temp_correct_index(1:end-1)+1]) temp_correct_length-1];
                    temp_correct=temp_correct(temp_correct(:,2)>=mintime,:);
                    
                    %Caluculate how far fly walks in safe tile each time it goes in
                    temp_correct(:,3)=0;
                    for temp_i=1:length(temp_correct(:,1))
                        for temp_j=temp_correct(temp_i,1):(temp_correct(temp_i,1)+temp_correct(temp_i,2)-2)
                            temp_correct(temp_i,3)=temp_correct(temp_i,3)+pdist([temp_data(temp_j,xcolumn(fly):ycolumn(fly)); temp_data(temp_j+1,xcolumn(fly):ycolumn(fly))],'euclidean');
                        end
                    end
                end
                
                %Find all frames when fly is outside the safe tile for at
                %least mintime, how long they were there for and how much
                %they walked.
                %Columns:
                %(1) Frame entered incorrect tile
                %(2) Number of frames until leaving safe tile again
                %(3) Is current unsafe tile prevsafe (0) or not (1)
                %(4) Distance walked before leaving safe tile
                temp_incorrect=find((temp_data(:,xcolumn(fly))<temp_l_edge | temp_data(:,xcolumn(fly))>temp_r_edge)==1);
                if ~isempty(temp_incorrect)
                    temp_incorrect_length=diff(temp_incorrect);
                    temp_incorrect_length=find([temp_incorrect_length; inf]>1);
                    temp_incorrect_length=diff([0; temp_incorrect_length]);
                    temp_incorrect_index=cumsum(temp_incorrect_length);
                    temp_incorrect=[temp_incorrect([1; temp_incorrect_index(1:end-1)+1]) temp_incorrect_length-1];
                    temp_incorrect=temp_incorrect(temp_incorrect(:,2)>=mintime,:);
                    
                    %Checks if the current unsafe tile is prevsafe or not
                    temp_incorrect(:,3)=temp_data(temp_incorrect(:,1),xcolumn(fly));
                    temp_incorrect(temp_incorrect(:,3)>=temp_l_edgeprev & temp_incorrect(:,3)<=temp_r_edgeprev,3)=-2;
                    temp_incorrect(temp_incorrect(:,3)~=-2,3)=-1;
                    temp_incorrect(:,3)=temp_incorrect(:,3)+2;
                    
                    %Caluculate how far fly walks in safe tile each time it goes in
                    temp_incorrect(:,4)=0;
                    for temp_i=1:length(temp_incorrect(:,1))
                        for temp_j=temp_incorrect(temp_i,1):(temp_incorrect(temp_i,1)+temp_incorrect(temp_i,2)-2)
                            temp_incorrect(temp_i,4)=temp_incorrect(temp_i,4)+pdist([temp_data(temp_j,xcolumn(fly):ycolumn(fly)); temp_data(temp_j+1,xcolumn(fly):ycolumn(fly))],'euclidean');
                        end
                    end
                end
                
                %Calculate how long the fly was in previously safe and unsafe time before reaching safe
                if ~isempty(temp_correct)
                    if curr_phase>1
                        temp_prevsafebeforesafe=find((temp_data(1:temp_correct(1)-1,xcolumn(fly))>=temp_l_edgeprev & temp_data(1:temp_correct(1)-1,xcolumn(fly))<=temp_r_edgeprev)==1);
                        temp_unsafebeforesafe=find((temp_data(1:temp_correct(1)-1,xcolumn(fly))<temp_l_edgeprev | temp_data(1:temp_correct(1)-1,xcolumn(fly))>temp_r_edgeprev)==1);
                    else
                        temp_prevsafebeforesafe=[];
                        temp_unsafebeforesafe=find((temp_data(1:temp_correct(1)-1,xcolumn(fly))<temp_l_edge | temp_data(1:temp_correct(1)-1,xcolumn(fly))>temp_r_edge)==1);
                    end
                else
                    if curr_phase>1
                        temp_prevsafebeforesafe=find((temp_data(1:end,xcolumn(fly))>=temp_l_edgeprev & temp_data(1:end,xcolumn(fly))<=temp_r_edgeprev)==1);
                        temp_unsafebeforesafe=find((temp_data(1:end,xcolumn(fly))<temp_l_edgeprev | temp_data(1:end,xcolumn(fly))>temp_r_edgeprev)==1);
                    else
                        temp_prevsafebeforesafe=[];
                        temp_unsafebeforesafe=find((temp_data(1:end,xcolumn(fly))<temp_l_edge | temp_data(1:end,xcolumn(fly))>temp_r_edge)==1);
                    end
                end
                
                temp_speed_x = [0; hypot(diff(temp_data(:,xcolumn(fly))), diff(temp_data(:,ycolumn(fly))))];
                
                %Save time spent and mean speed in safe tile
                if ~isempty(temp_correct)
                    phase(curr_phase,4) = sum(temp_correct(:,2))/fps;
                    phase(curr_phase,8) = nanmean(temp_speed_x((temp_data(:,xcolumn(fly))>=temp_l_edge & temp_data(:,xcolumn(fly))<=temp_r_edge)==1))*fps*pixelcmratio;
                else
                    phase(curr_phase,4) = 0;
                    phase(curr_phase,8) = NaN;
                end
                
                %Save time spent and mean speed in prev safe tile before entering safe
                if ~isempty(temp_prevsafebeforesafe)
                    phase(curr_phase,5) = length(temp_prevsafebeforesafe)/fps;
                    phase(curr_phase,9) = nanmean(temp_speed_x(temp_prevsafebeforesafe))*fps*pixelcmratio;
                else
                    phase(curr_phase,5) = 0;
                    phase(curr_phase,9) = NaN;
                end
                
                %Save time spent and mean speed in unsafe tile before entering safe
                if ~isempty(temp_unsafebeforesafe)
                    phase(curr_phase,6) = length(temp_unsafebeforesafe)/fps;
                    phase(curr_phase,10) = nanmean(temp_speed_x(temp_unsafebeforesafe))*fps*pixelcmratio;
                else
                    phase(curr_phase,6) = 0;
                    phase(curr_phase,10) = NaN;
                end
                                
                %Calculate averave fly speed in phase
                phase(curr_phase,7) = nanmean([0; hypot(diff(temp_data(:,xcolumn(fly))), diff(temp_data(:,ycolumn(fly))))])*fps*pixelcmratio;
                
                %Calculate and save total distance walked in phase
                for temp_i=1:length(temp_data(:,1))-1
                    phase(curr_phase,11)=phase(curr_phase,11)+pdist([temp_data(temp_i,xcolumn(fly):ycolumn(fly)); temp_data(temp_i+1,xcolumn(fly):ycolumn(fly))],'euclidean');
                end
                phase(curr_phase,11)=phase(curr_phase,11)*pixelcmratio;
                
                %If fly has entered the safe tile, calculate and save variables. Otherwise set them to NaN
                if ~isempty(temp_correct)
                    
                    %Calculate distance traveled before reaching safe tile for the first time
                    temp_disttosafe=0;
                    for temp_i=1:temp_correct(1)-1
                        temp_disttosafe=temp_disttosafe+pdist([temp_data(temp_i,xcolumn(fly):ycolumn(fly)); temp_data(temp_i+1,xcolumn(fly):ycolumn(fly))],'euclidean');
                    end
                    %Save distance traveled before reaching safe tile for the first time
                    phase(curr_phase,12)=temp_disttosafe*pixelcmratio;
                else
                    phase(curr_phase,12)=NaN;
                end
                
                %Save current safe tile for next phase calculations
                temp_l_edgeprev=temp_l_edge;
                temp_r_edgeprev=temp_r_edge;
                
                clearvars temp* -except *prev
            end %end phase
            
            %% Per fly per video calculations
            
            %Save variables to disk, clear all memory, and reload variables
            save('vars.mat')
            close all
            clear all
            clear java
            load('vars.mat')
            delete('vars.mat')
            
            %Check for dead flies
            if length(Data)>=phases(end,1)+1+phases(end,3)*fps
                x_time=phases(end,1)+1+phases(end,3)*fps;
            else
                x_time=length(Data);
            end
            
            x_left=diff([0; find([diff(Data(Data(1:x_time,xcolumn(1:numflies))<=tile_l,1)); inf]>1)]);
            x_middle=diff([0; find([diff(Data(Data(1:x_time,xcolumn(1:numflies))>tile_l & Data(1:x_time,xcolumn(1:numflies))<tile_r,1)); inf]>1)]);
            x_right=diff([0; find([diff(Data(Data(1:x_time,xcolumn(1:numflies))>=tile_r,1)); inf]>1)]);
            x_max=max([x_left; x_middle; x_right]);
            
            if x_max>(secondstodead)*fps && nullifydeadflies==1
                phase(:,2:end)=NaN;
            end
            
            %% Saves per fly per video results into csvs
            if exist('phase','var')
                csvwrite([root{condition} int2str(video) '_' int2str(fly) '_analysis.csv'],phase);
            end
            
            clearvars temp* x_* probesfly
            
            %% Creates 3D array with all results of one condition so they can be averaged together
            
            eval(['result_' strrep(root{condition},'_','') '=cat(3, result_' strrep(root{condition},'_','') ',phase);']);
            
        end %end fly
        
        clearvars *fly phases h p x y ynext
        
    end %end video
    
    clearvars curr* Data video fly
    
    %% Average all flies and videos per condition together, plot and save
    
    %Get the results for all videos of current condition
    eval(['rplot=result_' strrep(root{condition},'_','') ';']);
    
    %Average all videos and flies and save as a csv
    eval(['result=nanmean(result_' strrep(root{condition},'_','') ',3);']);
    eval(['result_' strrep(root{condition},'_','') '=result;']);
    csvwrite([strrep(root{condition},'_','') '_mean.csv'],result);
    
    result_all=cat(3,result_all,result);
    
end % end condition

%Play sound to alert program has ended
load handel;
sound(y(2000:17500),Fs);
pause(2)
clear all