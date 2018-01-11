%% ----- Code by Andrea Soto Padilla ----- %%
% ------ Date: 15 May 2016 ------ %
% ------ Modified by 
% Modifications: 

%% ------Output Description------
%Columns for per video and condition averages are the same
%Columns - Description
%[01] - Safe tile [0 - Pole, 1 - Left, 2 - Right]
%[02] - Was fly already in safe tile at the beginning of trial [0 - No, 1 - Yes]
%[03] - Time in seconds which fly started moving (to calculate time to start moving)
%[04] - Time in seconds it took to get to the safe tile 
%[05] - Distance in pixels traveled to reach safe tile
%[06] - Total time  spent in safe tile
%[07] - Number of times probed (times out safe once reached)
%[08] - Mean duration of probes
%[09] - Mean distance walked while probing
%[10] - Total distance walked in phase
%[11] - First choice [0 - Incorrect, 1 - Correct]
%[12] - Went to previously safe tile [0 - No, 1 - Yes]
%[13] - Went to closest tile [0 - No, 1 - Yes]
%[14] - Total time in seconds spent in unsafe tile (after going to safe tile)
%[15] - Total time in seconds spent in pole position (after going to safe tile)
%[16] - Average speed during trial
%[17] - Time in seconds the fly was in pole before getting to the safe tile for the first time
%[18] - Time in seconds the fly was in unsafe tile before getting to the safe tile for the first time

%% Clear memory and worspace
clear all
clear java
clc

%% -------Define Variables-------
conditions={'FC'}; %name for conditions (with underscore)
flies={''}; %name for flies (with underscore)
filesuffix='_output'; %end name of file
ledtype=3; %set LED type [1 - Blink with PP lights, 2 - Blink no PP lights, 3 - Constant no PP lights]
poles='Yes'; %Set 'Yes' or 'No' for whether the experiment has pole position or not
poletile=1; %set PP tile [1 - left tile, 2 - middle tile, 3 - right tile]
invertvid=0; %Set video range (i.e. 1:10 or 11:20) to have x coordinates inverted (left to right). Set to 0 for no inversion
arenax=[281 1058]; %left and right edges of video
arenay=[343 586]; %top and bottom edges of video
fps=30; %frames per second of video
trialduration=60; %duration of trial in seconds
numvideos=20; %number of videos in experiment for each condition
numphases=60; %number of phases (phase = pole and trial)
numflies=1; %number of flies to be tracked
mindist=0; %minimum distance from edge fly must be in tile to be considered (in)correct
minmove=5; %minimum distance in pixels to condire the fly has moved
mintime=0; %minimum number of frames fly must be in tile to be considered (in)correct
%% ------------------------------

%Set columns for x and y position data for multiple flies
xcolumn=4:2:14;
ycolumn=xcolumn+1;

%Find tile edges
tile_l=diff(arenax)/3;
tile_r=2*tile_l;

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
    for video=1:numvideos
        
        fprintf('Checking video %s out of %s for condition %s.\n',int2str(video),int2str(numvideos),strrep(root{condition},'_',''));
        
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
        
        %Invert x values for videos specified above
        if max(invertvid)~=0 && ismember(video, invertvid)
            Data(:,xcolumn(1:numflies))=diff(arenax)-Data(:,xcolumn(1:numflies));
        end
        
        %Determined phases from LEDs based on LED Type set above
        
        %Find where the LEDs are on
        led_left=find(Data(:,2)==1);
        led_right=find(Data(:,3)==1);
        
        %Determine frames for poles and trials
        if ledtype~=3 %led type 1 and 2
            
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
                    pole=sort([left; right]-trialduration*fps);
                else
                    pole=[];
                end
            end
            
        else %LED type 3
            %Find where left LEDs are turned on
            led_left=find(Data(:,2)==1);
            if ~isempty(led_left)
                %Find how long LEDs are on for
                led_left_length=diff(led_left);
                led_left_begin=find([inf; led_left_length;]>100);
                led_left_end=find([led_left_length; inf]>100);
                left=led_left(led_left_begin);
            end
            
            %Find where right LEDs are turned on
            led_right=find(Data(:,3)==1);
            if ~isempty(led_right)
                %Find how long LEDs are on for
                led_right_length=diff(led_right);
                led_right_begin=find([inf; led_right_length]>100);
                led_right_end=find([led_right_length; inf]>100);
                right=led_right(led_right_begin);
            end
            
            %Find the poles
            if (~isempty(led_left) || ~isempty(led_right)) && strcmp(poles, 'Yes')
                pole=sort([1; led_left(led_left_end)+1; led_right(led_right_end)+1]);
                pole=pole(1:end-1);
            else
                pole=[];
            end
            
        end
        
        %make one vector with all the poles and trials in order (phases - pole=0, left=1, right=2)
        phases=sort([pole; left; right]);
        phases(ismember(phases,pole),2)=0;
        phases(ismember(phases,left),2)=1;
        phases(ismember(phases,right),2)=2;
        
        clearvars led_* left right pole
        
        if length(phases)<numphases
            csvwrite([root{condition} int2str(video) filesuffix '_only_' int2str(length(phases)) '_phases.csv'],[]);
            stop=1;
            pause(0.25);
            clc
            continue
        end
        clc
    end
end

clearvars condition video phases temp*

%Stop execution if a problem was found
if stop==1
    clc
    fprintf('A problem was found with one of the files. Please check CSVs for more details.\n');
    beep on
    beep
    clear all
    break
end

%% Start Analysis

%Initialize some variables
result_all=[];

for condition=1:length(root)
    
    %Initialize some variables
    eval(['result_' root{condition}(1:end-1) '=[];']);
    xys=[];
    
    for video=1:numvideos
        
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
        
        %Invert x values for videos specified above
        if max(invertvid)~=0 && ismember(video, invertvid)
            Data(:,xcolumn(1:numflies))=diff(arenax)-Data(:,xcolumn(1:numflies));
        end
        
        %% Determined phases from LEDs based on LED Type set above
        
        %Find where the LEDs are on
        led_left=find(Data(:,2)==1);
        led_right=find(Data(:,3)==1);
        
        %Determine frames for poles and trials
        if ledtype~=3 %led type 1 and 2
            
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
                    pole=sort([left; right]-trialduration*fps);
                else
                    pole=[];
                end
            end
            
        else %LED type 3
            %Find where left LEDs are turned on
            led_left=find(Data(:,2)==1);
            if ~isempty(led_left)
                %Find how long LEDs are on for
                led_left_length=diff(led_left);
                led_left_begin=find([inf; led_left_length;]>100);
                led_left_end=find([led_left_length; inf]>100);
                left=led_left(led_left_begin);
            end
            
            %Find where right LEDs are turned on
            led_right=find(Data(:,3)==1);
            if ~isempty(led_right)
                %Find how long LEDs are on for
                led_right_length=diff(led_right);
                led_right_begin=find([inf; led_right_length]>100);
                led_right_end=find([led_right_length; inf]>100);
                right=led_right(led_right_begin);
            end
            
            %Find the poles
            if (~isempty(led_left) || ~isempty(led_right)) && strcmp(poles, 'Yes')
                pole=sort([1; led_left(led_left_end)+1; led_right(led_right_end)+1]);
                pole=pole(1:end-1);
            else
                pole=[];
            end
            
        end
        
        %make one vector with all the poles and trials in order (phases - pole=0, left=1, right=2)
        phases=sort([pole; left; right]);
        phases(ismember(phases,pole),2)=0;
        phases(ismember(phases,left),2)=1;
        phases(ismember(phases,right),2)=2;
        
        clearvars led_* left right pole
        
        if length(phases)<numphases
            csvwrite([root{condition} int2str(video) filesuffix '_only_' int2str(length(phases)) '_phases.csv'],[]);
            continue
        end
        
        %initialize some variables
        curr_phase=1;
        phase=zeros(length(phases),15);
        
        for fly=1:numflies
            %% Start per phase calculations (phase = Pole Position or Trial)
            for curr_safe=1:length(phases)
                
                %Saves which tile is safe for the current trial
                phase(curr_phase,1)=phases(curr_safe,2);
                
                %Loads the data only for the current phase
                if curr_safe<length(phases)
                    temp_data=Data(phases(curr_safe):phases(curr_safe+1)-1,:);
                else
                    temp_data=Data(phases(curr_safe)+1:phases(curr_safe)+1+trialduration*fps,:);
                end
                
                %Set the left and right edges for the safe tile according to current phase and which tile is the pole position
                if (poletile==1 && phases(curr_safe,2)==0) || (poletile~=1 && phases(curr_safe,2)==1) %left tile
                    temp_l_edge=0;
                    temp_r_edge=tile_l;
                elseif (poletile==1 && phases(curr_safe,2)==1) || (poletile==2 && phases(curr_safe,2)==0) || (poletile==3 && phases(curr_safe,2)==2) %middle tile
                    temp_l_edge=tile_l;
                    temp_r_edge=tile_r;
                elseif (poletile==3 && phases(curr_safe,2)==0) || (poletile~=3 && phases(curr_safe,2)==2) %right tile
                    temp_l_edge=tile_r;
                    temp_r_edge=diff(arenax);
                end
                
                %Set left and right edges for pole position tile
                if poletile==1
                    temp_l_pole=0;
                    temp_r_pole=tile_l;
                elseif poletile==2
                    temp_l_pole=tile_l;
                    temp_r_pole=tile_r;
                elseif poletile==3
                    temp_l_pole=tile_r;
                    temp_r_pole=diff(arenax);
                end
                
                %Shift tile edges in by mindist
                temp_l_edge=temp_l_edge+mindist;
                temp_r_edge=temp_r_edge-mindist;
                temp_l_pole=temp_l_pole-mindist;
                temp_r_pole=temp_r_pole+mindist;
                
                %Check closest tile to start position (1 - left, 2 - middle, 3 - right)
                temp_startx=temp_data(1,xcolumn(fly));
                if temp_startx>=tile_l && temp_startx<diff(arenax)/2
                    temp_closesttile=0;
                elseif temp_startx<=tile_r && temp_startx>diff(arenax)/2
                    temp_closesttile=3;
                else
                    temp_closesttile=2;
                end
                
                
                %Flag for whether fly is in pole position (0) safe tile (1) or unsafe tile (2) at the beginning of the phase
                if temp_data(1,xcolumn(fly))>temp_l_pole && temp_data(1,xcolumn(fly))<temp_r_pole
                    phase(curr_phase,2)=0;
                elseif temp_data(1,xcolumn(fly))>=temp_l_edge && temp_data(1,xcolumn(fly))<=temp_r_edge
                    if phases(curr_phase,1)==0
                        phase(curr_phase,2)=0;
                    else
                        phase(curr_phase,2)=1;
                    end
                else
                    phase(curr_phase,2)=2;
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
                    temp_correct=[temp_correct([1; temp_correct_index(1:end-1)+1]) temp_correct_length];
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
                %(1) Frame entered correct tile
                %(2) Number of frames until leaving safe tile again
                %(3) Is current insafe tile pole (0) or not (1)
                %(4) Distance walked before leaving safe tile
                temp_incorrect=find((temp_data(:,xcolumn(fly))<temp_l_edge | temp_data(:,xcolumn(fly))>temp_r_edge)==1);
                if ~isempty(temp_incorrect)
                    temp_incorrect_length=diff(temp_incorrect);
                    temp_incorrect_length=find([temp_incorrect_length; inf]>1);
                    temp_incorrect_length=diff([0; temp_incorrect_length]);
                    temp_incorrect_index=cumsum(temp_incorrect_length);
                    temp_incorrect=[temp_incorrect([1; temp_incorrect_index(1:end-1)+1]) temp_incorrect_length];
                    temp_incorrect=temp_incorrect(temp_incorrect(:,2)>=mintime,:);
                    
                    %Checks if the current unsafe tile is pole or not
                    temp_incorrect(:,3)=temp_data(temp_incorrect(:,1),xcolumn(fly));
                    temp_incorrect(temp_incorrect(:,3)>=temp_l_pole & temp_incorrect(:,3)<=temp_r_pole,3)=-2;
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
                
                %If fly has entered the safe tile, calculate and save variables. Otherwise set them to NaN
                if ~isempty(temp_correct)
                    
                    %Save first frame when fly entered the safe tile
                    phase(curr_phase,4)=temp_correct(1)/fps;
                    
                    %Calculate distance traveled before reaching safe tile for the first time
                    temp_disttosafe=0;
                    for temp_i=1:temp_correct(1)-1
                        temp_disttosafe=temp_disttosafe+pdist([temp_data(temp_i,xcolumn(fly):ycolumn(fly)); temp_data(temp_i+1,xcolumn(fly):ycolumn(fly))],'euclidean');
                    end
                    %Save distance traveled before reaching safe tile for the first time
                    phase(curr_phase,5)=temp_disttosafe;
                    
                    %Save total time spent in the safe tile
                    phase(curr_phase,6)=sum(temp_correct(:,2))/fps;
                    
                    %if fly was already in safe tile, set frame entered, distance traveled and time spent to NaN
                    if phase(curr_phase,2)==1
                        phase(curr_phase,4)=NaN;
                        phase(curr_phase,5)=NaN;
                        phase(curr_phase,6)=NaN;
                    end
                    
                    %Checks which times fly was in unsafe tile is a probe
                    if ~isempty(temp_incorrect)
                        if phase(curr_phase,2)==1
                            phase(curr_phase,7)=length(temp_incorrect(:,1));
                            phase(curr_phase,8)=mean(temp_incorrect(:,2))/fps;
                            phase(curr_phase,9)=mean(temp_incorrect(:,4));
                        else
                            phase(curr_phase,7)=length(temp_incorrect(2:end,1));
                            phase(curr_phase,8)=mean(temp_incorrect(2:end,2));
                            phase(curr_phase,9)=mean(temp_incorrect(2:end,4));
                        end
                    else
                        phase(curr_phase,7)=NaN;
                        phase(curr_phase,8)=NaN;
                        phase(curr_phase,9)=NaN;
                    end
                    
                else
                    phase(curr_phase,4)=NaN;
                    phase(curr_phase,5)=NaN;
                    phase(curr_phase,6)=NaN;
                    phase(curr_phase,7)=NaN;
                    phase(curr_phase,8)=NaN;
                    phase(curr_phase,9)=NaN;
                end
                
                %Calculate and save total distance walked in phase
                for temp_i=1:length(temp_data(:,1))-1
                    phase(curr_phase,10)=phase(curr_phase,10)+pdist([temp_data(temp_i,xcolumn(fly):ycolumn(fly)); temp_data(temp_i+1,xcolumn(fly):ycolumn(fly))],'euclidean');
                end
                
                %Calculate averave fly speed in phase
                phase(curr_phase,16) = nanmean([0; hypot(diff(temp_data(:,xcolumn(fly))), diff(temp_data(:,ycolumn(fly))))])*fps;
                
                %Calculate trial only variables
                if phases(curr_safe,2)~=0
                    
                    %Check if first choice was safe tile (1) or not (0)
                    if phase(curr_phase,2)==0
                        if ~isempty(temp_correct) && phase(curr_phase,2)~=1 && (isempty(temp_incorrect) || length(temp_incorrect(:,1))<2 || temp_correct(1)<temp_incorrect(2))
                            phase(curr_phase,11)=1;
                        end
                    else
                        phase(curr_phase,11)=0;
                    end
                    
                    %Check if first choice was same as previous safe tile (1) or not (0). Set NaN for first trial.
                    if curr_phase>2 && phase(curr_phase,11)==1
                        if phases(curr_phase,1)==phases(curr_phase-2,1)
                            phase(curr_phase,12)=1;
                        else
                            phase(curr_phase,12)=0;
                        end
                    elseif curr_phase>2 && phase(curr_phase,11)==0
                        if phase(curr_phase,1)==phase(curr_phase-2,1)
                            phase(curr_phase,12)=0;
                        else
                            phase(curr_phase,12)=1;
                        end
                    else
                        phase(curr_phase,12)=NaN;
                    end
                    
                    %Check if first choice was the closest tile fly was to at begining of phase (1) or not (0)
                    if poletile==2
                        if phases(curr_phase,1)==1 && phase(curr_phase,11)==1 && temp_closesttile==1
                            phase(curr_phase,13)=1;
                        elseif phases(curr_phase,1)==1 && phase(curr_phase,11)==1 && temp_closesttile~=1
                            phase(curr_phase,13)=0;
                        elseif phases(curr_phase,1)==1 && phase(curr_phase,11)==0 && temp_closesttile==1
                            phase(curr_phase,13)=0;
                        elseif phases(curr_phase,1)==1 && phase(curr_phase,11)==0 && temp_closesttile~=1
                            phase(curr_phase,13)=1;
                        elseif phases(curr_phase,1)==2 && phase(curr_phase,11)==1 && temp_closesttile==3
                            phase(curr_phase,13)=1;
                        elseif phases(curr_phase,1)==2 && phase(curr_phase,11)==1 && temp_closesttile~=3
                            phase(curr_phase,13)=0;
                        elseif phases(curr_phase,1)==2 && phase(curr_phase,11)==0 && temp_closesttile==3
                            phase(curr_phase,13)=0;
                        elseif phases(curr_phase,1)==2 && phase(curr_phase,11)==0 && temp_closesttile~=3
                            phase(curr_phase,13)=1;
                        end
                    else
                        phase(curr_phase,13)=NaN;
                    end
                    
                    %Calculates total time in unsafe and pole tiles
                    if ~isempty(temp_incorrect)
                        if phase(curr_phase,2)==1
                            phase(curr_phase,14)=sum(temp_incorrect(temp_incorrect(:,3)==1,2))/fps;
                            phase(curr_phase,15)=sum(temp_incorrect(temp_incorrect(:,3)==0,2))/fps;
                        else
                            temp_incorrect=temp_incorrect(2:end,:);
                            phase(curr_phase,14)=sum(temp_incorrect(temp_incorrect(:,3)==1,2))/fps;
                            phase(curr_phase,15)=sum(temp_incorrect(temp_incorrect(:,3)==0,2))/fps;
                        end
                    else
                        phase(curr_phase,14)=NaN;
                        phase(curr_phase,15)=NaN;
                    end
                    
                    %Calculate how long the fly was in pole and unsafe time before reaching safe
                    if ~isempty(temp_correct)
                        temp_beforesafe=temp_data(1:temp_correct(1)-1,xcolumn(fly));
                        phase(curr_phase,17)=sum(temp_beforesafe>temp_l_pole & temp_beforesafe<temp_r_pole)/fps;
                        phase(curr_phase,18)=sum(~(temp_beforesafe>temp_l_pole & temp_beforesafe<temp_r_pole))/fps;
                    else
                        phase(curr_phase,17)=NaN;
                        phase(curr_phase,18)=NaN;
                    end
                    
                else %Set trial only variables to NaN in pole
                    phase(curr_phase,11)=NaN;
                    phase(curr_phase,12)=NaN;
                    phase(curr_phase,13)=NaN;
                    phase(curr_phase,14)=NaN;
                    phase(curr_phase,15)=NaN;
                    phase(curr_phase,17)=NaN;
                    phase(curr_phase,18)=NaN;
                end
                
                curr_phase=curr_phase+1;
                
                clearvars temp*
            end %end phase
            
            %% Per fly per video calculations
            
            %Calculate fly speed between frames for entire video
            speed = [0; hypot(diff(Data(:,xcolumn(fly))), diff(Data(:,ycolumn(fly))))];
            
            %Build one matrix with all X Y coordinates and speed of all videos in contdition per frame
            xys=[xys; Data(:,xcolumn(fly)) Data(:,ycolumn(fly)) speed];
            %Remove velocities above 10 times mean speed and aggregate
            %by X Y coordinate
            xys(xys(:,3)>=nanmean(xys(:,3))*10,3)=NaN;
            [b, ~, n] = unique(xys(:,1:2) , 'rows');
            xys2  = accumarray(n , xys(:,3) , [] , @(x) mode(x));
            xys  = cat(2 , b , xys2);
            
            clearvars xyv2 b n
            
            %% Plot fly track with correct and incorrect phases
            
            plot(Data(:,xcolumn(fly)),Data(:,1));
            hold on;
            plot([tile_l, tile_l],[1,Data(end,1)]);
            hold on;
            plot([tile_r, tile_r],[1,Data(end,1)]);
            
            for p=1:length(phases)
                if p<length(phases)
                    ynext=phases(p+1,1);
                else
                    ynext=2*phases(end,1)-phases(end-1,1);
                end
                y=[phases(p,1), phases(p,1), ynext, ynext];
                
                if poletile==1
                    if phase(p,1)==0
                        x=[0, tile_l, tile_l, 0];
                    elseif phase(p,1)==1
                        x=[tile_l, tile_r, tile_r, tile_l];
                    elseif phase(p,1)==2
                        x=[tile_r, diff(arenax), diff(arenax), tile_r];
                    end
                elseif poletile==2
                    if phase(p,1)==0
                        x=[tile_l, tile_r, tile_r, tile_l];
                    elseif phase(p,1)==1
                        x=[0, tile_l, tile_l, 0];
                    elseif phase(p,1)==2
                        x=[tile_r, diff(arenax), diff(arenax), tile_r];
                    end
                elseif poletile==3
                    if phase(p,1)==0
                        x=[tile_r, diff(arenax), diff(arenax), tile_r];
                    elseif phase(p,1)==1
                        x=[0, tile_l, tile_l, 0];
                    elseif phase(p,1)==2
                        x=[tile_l, tile_r, tile_r, tile_l];
                    end
                end
                
                if phase(p,11)==1 || ((phase(p,1)==0 && phase(p,2)~=0) && ~isnan(phase(p,4)))
                    color='g';
                else
                    color='r';
                end
                
                h=fill(x,y,color);
                set(h,'EdgeColor','None','facealpha',.25);
                hold on;
            end
            
            title([strrep(root{condition},'_',' ') int2str(video) ' ' int2str(fly) ' Track']);
            xlabel('X Position (pixels)');
            xlim([0, diff(arenax)]);
            ylabel('Time (frames)');
            ylim([1,ynext]);
            saveas(gcf, [root{condition} int2str(video) '_' int2str(fly) '_track'], 'png');
            close all
            
            %% Saves per fly per video results into csvs
            if exist('phase','var')
                csvwrite([root{condition} int2str(video) '_' int2str(fly) '_analysis.csv'],phase);
            end
            
            %% Creates 3D array with all results of one condition so they can be averaged together
            
            %if pole position is not middle tile, then group by phase
            if poletile~=2
                phase=[phase(phase(:,1)==0,:); phase(phase(:,1)==1,:); phase(phase(:,1)==2,:)];
            end
            
            eval(['result_' root{condition}(1:end-1) '=cat(3, result_' root{condition}(1:end-1) ',phase);']);
            
        end %end fly
        
        clearvars *fly phases h p x y ynext
        
    end %end video
    
    clearvars curr* Data video fly
    
    %% Average all flies and videos per condition together, plot and save
    
    %Get the results for all videos of current condition
    eval(['rplot=result_' root{condition}(1:end-1) ';']);
    
    %Average all videos and flies and save as a csv
    eval(['result=nanmean(result_' root{condition}(1:end-1) ',3);']);
    eval(['result_' root{condition}(1:end-1) '=result;']);
    csvwrite(['result_' root{condition}(1:end-1) '_mean.csv'],result);
    
    result_all=cat(3,result_all,result);
    
    %Plots per condition
    
    %Create color map with one color per video
    c=hsv(size(rplot,3));
    
    %Scatter plot of X-Y coordinates for all videos and flies
    plot([tile_l tile_l],[1 max(arenay)],'Color','r');
    hold on;
    plot([tile_r tile_r],[1 max(arenay)],'Color','r');
    hold on
    plot(xys(:,1),xys(:,2),'o','MarkerSize',1,'Color',[0.5 0.5 0.5])
    title([strrep(root{condition},'_',' ') 'Fly Distribution']);
    xlabel('X Position (pixels)');
    xlim([0, diff(arenax)]);
    ylabel('Y Position (pixels)');
    ylim([0, diff(arenay)]);
    saveas(gcf, [root{condition}(1:end-1) '_flydistribution'], 'png');
    close all
 
    %Scatter plot of X-Y coordinates for all videos with speed as color    
    plot([tile_l tile_l],[1 max(arenay)],'Color','r');
    hold on;
    plot([tile_r tile_r],[1 max(arenay)],'Color','r');
    hold on
    scatter(xys(:,1),xys(:,2),1,xys(:,3))
    colormap parula;
    cbar=colorbar;
    ylabel(cbar,'Speed (pixels/second)');
    title([strrep(root{condition},'_',' ') 'Speed']);
    xlabel('X Position (pixels)');
    xlim([0, diff(arenax)]);
    ylabel('Y Position (pixels)');
    ylim([0, diff(arenay)]);
    
    saveas(gcf, [root{condition}(1:end-1) '_speed'], 'png');
    close all
    
    clearvars n b xyv* speed
    
    %Define what is the safe tile according to pole
    if poletile==1
        tside=[2 1];
        tsuffix={'Long', 'Short'};
    elseif poletile==2
        tside=0;
        tsuffix={'Safe'};
    elseif poletile==3
        tside=[1 2];
        tsuffix={'Long', 'Short'};
    end
    
    for tcnt=1:length(tside)
        
        %% Define trials phases
        if poletile==2
            itrial=rplot(:,1,:)~=0;
        else
            itrial=rplot(:,1,:)==tside(tcnt);
        end
        tindex=1:sum(itrial(:,:,1));
        tvid=ones(length(tindex),1);
        trial=[];
        for i=1:size(rplot,3)
            trial=[trial; rplot(itrial(:,:,i),:,i) tindex' tvid*i];
        end
        
        %Define pole phases
        ipole=rplot(:,1,:)==0;
        pindex=1:sum(ipole(:,:,1));
        pvid=ones(length(pindex));
        pole=[];
        for i=1:size(rplot,3)
            pole=[pole; rplot(ipole(:,:,i),:,i) pindex' pvid*i];
        end
        
        %Trial plots
 
        x=1:max(trial(:,size(rplot,2)+1));
        
        %Define labels for x-ticks
        for i=1:length(x)
            xtick{i}=num2str(x(i));
        end
        xtick=[' ', xtick, ' '];
        
        %Time to start moving plot
        %Plot regression line
        lm=fitlm(trial(:,size(rplot,2)+1),trial(:,3));
        plot(lm);
        hold on
        %Plot conditions per trial
        for i=1:size(trial,1)
            plot(trial(i,size(rplot,2)+1),trial(i,3),'o','Color',c(trial(i,size(rplot,2)+2),:));
            hold on
        end
        %Graph settings
        title(['Time to start moving to ' tsuffix{tcnt} ' Tile - ' root{condition}(1:end-1)]);
        legend();
        xlabel('Trials');
        xlim([0, length(x)+1]);
        set(gca,'XTick',0:length(x)+1,'XTickLabel',xtick)
        ylabel('Time (seconds)');
        saveas(gcf, ['Time_to_start_moving_to_' tsuffix{tcnt} '_tile_' root{condition}(1:end-1)], 'png');
        close all
        
        %Time to safe plot
        %Plot regression line
        lm=fitlm(trial(:,size(rplot,2)+1),trial(:,4));
        plot(lm);
        hold on
        %Plot conditions per trial
        for i=1:size(trial,1)
            plot(trial(i,size(rplot,2)+1),trial(i,4),'o','Color',c(trial(i,size(rplot,2)+2),:));
            hold on
        end
        %Graph settings
        title(['Time to ' tsuffix{tcnt} ' Tile - ' root{condition}(1:end-1)]);
        legend;
        xlabel('Trials');
        xlim([0, length(x)+1]);
        set(gca,'XTick',0:length(x)+1,'XTickLabel',xtick)
        ylabel('Time (seconds)');
        saveas(gcf, ['Time_to_' tsuffix{tcnt} '_tile_' root{condition}(1:end-1)], 'png');
        close all
        
        %Distance to safe plot
        %Plot regression line
        lm=fitlm(trial(:,size(rplot,2)+1),trial(:,5));
        plot(lm);
        hold on
        %Plot conditions per trial
        for i=1:size(trial,1)
            plot(trial(i,size(rplot,2)+1),trial(i,5),'o','Color',c(trial(i,size(rplot,2)+2),:));
            hold on
        end
        %Graph settings
        title(['Distance to ' tsuffix{tcnt} ' Tile - ' root{condition}(1:end-1)]);
        legend;
        xlabel('Trials');
        xlim([0, length(x)+1]);
        set(gca,'XTick',0:length(x)+1,'XTickLabel',xtick)
        ylabel('Distance (pixels)');
        saveas(gcf, ['Distance_to_' tsuffix{tcnt} '_tile_' root{condition}(1:end-1)], 'png');
        close all
        
        %Total time in safe tile plot
        %Plot regression line
        lm=fitlm(trial(:,size(rplot,2)+1),trial(:,6));
        plot(lm);
        hold on
        %Plot conditions per trial
        for i=1:size(trial,1)
            plot(trial(i,size(rplot,2)+1),trial(i,6),'o','Color',c(trial(i,size(rplot,2)+2),:));
            hold on
        end
        %Graph settings
        title(['Total Time Spent in ' tsuffix{tcnt} ' Tile - ' root{condition}(1:end-1)]);
        legend;
        xlabel('Trials');
        xlim([0, length(x)+1]);
        set(gca,'XTick',0:length(x)+1,'XTickLabel',xtick)
        ylabel('Time (seconds)');
        saveas(gcf, ['Total_time_spent_in_' tsuffix{tcnt} '_tile_' root{condition}(1:end-1)], 'png');
        close all
        
        %Number of probes plot
        %Plot regression line
        lm=fitlm(trial(:,size(rplot,2)+1),trial(:,7));
        plot(lm);
        hold on
        %Plot conditions per trial
        for i=1:size(trial,1)
            plot(trial(i,size(rplot,2)+1),trial(i,7),'o','Color',c(trial(i,size(rplot,2)+2),:));
            hold on
        end
        %Graph settings
        title(['Number of Probes out of ' tsuffix{tcnt} ' Tile - ' root{condition}(1:end-1)]);
        legend;
        xlabel('Trials');
        xlim([0, length(x)+1]);
        set(gca,'XTick',0:length(x)+1,'XTickLabel',xtick)
        ylabel('Number of Probes');
        saveas(gcf, ['Number_of_probes_out_of_' tsuffix{tcnt} '_tile_' root{condition}(1:end-1)], 'png');
        close all
        
        %Total time in unsafe tile plot
        %Plot regression line
        lm=fitlm(trial(:,size(rplot,2)+1),trial(:,14)+trial(:,15));
        plot(lm);
        hold on
        %Plot conditions per trial
        for i=1:size(trial,1)
            plot(trial(i,size(rplot,2)+1),trial(i,14)+trial(i,15),'o','Color',c(trial(i,size(rplot,2)+2),:));
            hold on
        end
        %Graph settings
        title(['Total Time Spent outside ' tsuffix{tcnt} ' Tile - ' root{condition}(1:end-1)]);
        legend;
        xlabel('Trials');
        xlim([0, length(x)+1]);
        set(gca,'XTick',0:length(x)+1,'XTickLabel',xtick)
        ylabel('Time (seconds)');
        saveas(gcf, ['Total_time_spent_outside_' tsuffix{tcnt} '_tile_' root{condition}(1:end-1)], 'png');
        close all
 
        %Total time in pole before entering safe tile plot
        %Plot regression line
        lm=fitlm(trial(:,size(rplot,2)+1),trial(:,17));
        plot(lm);
        hold on
        %Plot conditions per trial
        for i=1:size(trial,1)
            plot(trial(i,size(rplot,2)+1),trial(i,17),'o','Color',c(trial(i,size(rplot,2)+2),:));
            hold on
        end
        %Graph settings
        title(['Total Time Spent in Pole before entering ' tsuffix{tcnt} ' Tile - ' root{condition}(1:end-1)]);
        legend;
        xlabel('Trials');
        xlim([0, length(x)+1]);
        set(gca,'XTick',0:length(x)+1,'XTickLabel',xtick)
        ylabel('Time (seconds)');
        saveas(gcf, ['Total_time_spent_in_pole_before_entering_' tsuffix{tcnt} '_tile_' root{condition}(1:end-1)], 'png');
        close all
        
        %Total time in unsafe tile before entering safe tile plot
        %Plot regression line
        lm=fitlm(trial(:,size(rplot,2)+1),trial(:,18));
        plot(lm);
        hold on
        %Plot conditions per trial
        for i=1:size(trial,1)
            plot(trial(i,size(rplot,2)+1),trial(i,18),'o','Color',c(trial(i,size(rplot,2)+2),:));
            hold on
        end
        %Graph settings
        title(['Total Time Spent in Unsafe Tile before entering ' tsuffix{tcnt} ' Tile - ' root{condition}(1:end-1)]);
        legend;
        xlabel('Trials');
        xlim([0, length(x)+1]);
        set(gca,'XTick',0:length(x)+1,'XTickLabel',xtick)
        ylabel('Time (seconds)');
        saveas(gcf, ['Total_time_spent_in_unsafetile_before_entering_' tsuffix{tcnt} '_tile_' root{condition}(1:end-1)], 'png');
        close all
        
        clearvars x xtick
        
        %Pole plots
        x=1:max(pole(:,17));
        
        %Define labels for x-ticks
        for i=1:length(x)
            xtick{i}=num2str(x(i));
        end
        xtick=[' ', xtick, ' '];
        
        %%%% Add Pole plots here
        
        clearvars x xtick
        
    end %end plots
    
end % end condition

clearvars -except result_* root poletile fps

%% Combine conditions and plot

%Define what is the safe tile according to pole
if poletile==1
    tside=[2 1];
    tsuffix={'Long', 'Short'};
elseif poletile==2
    tside=0;
    tsuffix={'Safe'};
elseif poletile==3
    tside=[1 2];
    tsuffix={'Long', 'Short'};
end

%Remove underscore from condition names for legend
l=strrep(root,'_',' ');

%Create color map with one color per condition
c=hsv(size(result_all,3));

for tcnt=1:length(tside)
    
    %Define trials phases
    if poletile==2
        itrial=result_all(:,1,:)~=0;
    else
        itrial=result_all(:,1,:)==tside(tcnt);
    end
    for i=1:size(result_all,3)
        trial(:,:,i)=result_all(itrial(:,:,i),:,i);
    end
    
    %Define pole phases
    ipole=result_all(:,1,:)==0;
    for i=1:size(result_all,3)
        pole(:,:,i)=result_all(ipole(:,:,i),:,i);
    end
    
    %Trial plots
    x=1:size(trial,1);
    
    %Define labels for x-ticks
    for i=1:length(x)
        xtick{i}=num2str(x(i));
    end
    xtick=[' ', xtick, ' '];
    
    %Time to start moving plot
    %Plot regression line
    for i=1:size(trial,3)
        lm=fitlm(x,trial(:,3,i));
        plot(lm.Fitted,'Color',c(i,:));
        hold on
    end
    %Plot conditions per trial
    for i=1:size(trial,3)
        plot(x,trial(:,3,i),'o','Color',c(i,:));
        hold on
    end
    %Graph settings
    title(['Time to start moving to ' tsuffix{tcnt} ' Tile']);
    legend(l);
    xlabel('Trials');
    xlim([0, length(x)+1]);
    set(gca,'XTick',0:length(x)+1,'XTickLabel',xtick)
    ylabel('Average Time (seconds)');
    saveas(gcf, ['Time_to_start_moving_to_' tsuffix{tcnt} '_tile'], 'png');
    close all
    
    %Time to safe plot
    %Plot regression line
    for i=1:size(trial,3)
        lm=fitlm(x,trial(:,4,i));
        plot(lm.Fitted,'Color',c(i,:));
        hold on
    end
    %Plot conditions per trial
    for i=1:size(trial,3)
        plot(x,trial(:,4,i),'o','Color',c(i,:));
        hold on
    end
    %Graph settings
    title(['Time to ' tsuffix{tcnt} ' Tile']);
    legend(l);
    xlabel('Trials');
    xlim([0, length(x)+1]);
    set(gca,'XTick',0:length(x)+1,'XTickLabel',xtick)
    ylabel('Average Time (seconds)');
    saveas(gcf, ['Time_to_' tsuffix{tcnt} '_tile'], 'png');
    close all
    
    %Distance to safe plot
    %Plot regression line
    for i=1:size(trial,3)
        lm=fitlm(x,trial(:,5,i));
        plot(lm.Fitted,'Color',c(i,:));
        hold on
    end
    %Plot conditions per trial
    for i=1:size(trial,3)
        plot(x,trial(:,5,i),'o','Color',c(i,:));
        hold on
    end
    %Graph settings
    title(['Distance to ' tsuffix{tcnt} ' Tile']);
    legend(l);
    xlabel('Trials');
    xlim([0, length(x)+1]);
    set(gca,'XTick',0:length(x)+1,'XTickLabel',xtick)
    ylabel('Average Distance (pixels)');
    saveas(gcf, ['Distance_to_' tsuffix{tcnt} '_tile'], 'png');
    close all
    
    %Total time in safe tile plot
    %Plot regression line
    for i=1:size(trial,3)
        lm=fitlm(x,trial(:,6,i));
        plot(lm.Fitted,'Color',c(i,:));
        hold on
    end
    %Plot conditions per trial
    for i=1:size(trial,3)
        plot(x,trial(:,6,i),'o','Color',c(i,:));
        hold on
    end
    %Graph settings
    title(['Total Time Spent in ' tsuffix{tcnt} ' Tile']);
    legend(l);
    xlabel('Trials');
    xlim([0, length(x)+1]);
    set(gca,'XTick',0:length(x)+1,'XTickLabel',xtick)
    ylabel('Average Time (seconds)');
    saveas(gcf, ['Total_time_spent_in_' tsuffix{tcnt} '_tile'], 'png');
    close all
    
    %Number of probes plot
    %Plot regression line
    for i=1:size(trial,3)
        lm=fitlm(x,trial(:,7,i));
        plot(lm.Fitted,'Color',c(i,:));
        hold on
    end
    %Plot conditions per trial
    for i=1:size(trial,3)
        plot(x,trial(:,7,i),'o','Color',c(i,:));
        hold on
    end
    %Graph settings
    title(['Number of Probes out of ' tsuffix{tcnt} ' Tile']);
    legend(l);
    xlabel('Trials');
    xlim([0, length(x)+1]);
    set(gca,'XTick',0:length(x)+1,'XTickLabel',xtick)
    ylabel('Average Number of Probes');
    saveas(gcf, ['Number_of_probes_out_of_' tsuffix{tcnt} '_tile'], 'png');
    close all
    
    %Total time in unsafe tile plot (after reaching safe - incorrect + pole tile together)
    %Plot regression line
    for i=1:size(trial,3)
        lm=fitlm(x,trial(:,14,i)+trial(:,15,i));
        plot(lm.Fitted,'Color',c(i,:));
        hold on
    end
    %Plot conditions per trial
    for i=1:size(trial,3)
        plot(x,trial(:,14,i)+trial(:,15,i),'o','Color',c(i,:));
        hold on
    end
    %Graph settings
    title(['Total Time Spent outside ' tsuffix{tcnt} ' Tile']);
    legend(l);
    xlabel('Trials');
    xlim([0, length(x)+1]);
    set(gca,'XTick',0:length(x)+1,'XTickLabel',xtick)
    ylabel('Average Time (seconds)');
    saveas(gcf, ['Total_time_spent_outside_' tsuffix{tcnt} '_tile'], 'png');
    close all
    
    %Total time in pole before entering safe tile plot
    %Plot regression line
    for i=1:size(trial,3)
        lm=fitlm(x,trial(:,17,i));
        plot(lm.Fitted,'Color',c(i,:));
        hold on
    end
    %Plot conditions per trial
    for i=1:size(trial,3)
        plot(x,trial(:,17,i),'o','Color',c(i,:));
        hold on
    end
    %Graph settings
    title(['Total Time Spent in Pole before entering ' tsuffix{tcnt} ' Tile']);
    legend(l);
    xlabel('Trials');
    xlim([0, length(x)+1]);
    set(gca,'XTick',0:length(x)+1,'XTickLabel',xtick)
    ylabel('Average Time (seconds)');
    saveas(gcf, ['Total_time_spent_in_pole_before_entering_' tsuffix{tcnt} '_tile'], 'png');
    close all
    
    %Total time in unsafe before entering safe tile plot
    %Plot regression line
    for i=1:size(trial,3)
        lm=fitlm(x,trial(:,18,i));
        plot(lm.Fitted,'Color',c(i,:));
        hold on
    end
    %Plot conditions per trial
    for i=1:size(trial,3)
        plot(x,trial(:,18,i),'o','Color',c(i,:));
        hold on
    end
    %Graph settings
    title(['Total Time Spent in Unsafe Tile before entering ' tsuffix{tcnt} ' Tile']);
    legend(l);
    xlabel('Trials');
    xlim([0, length(x)+1]);
    set(gca,'XTick',0:length(x)+1,'XTickLabel',xtick)
    ylabel('Average Time (seconds)');
    saveas(gcf, ['Total_time_spent_in_unsafetile_before_entering_' tsuffix{tcnt} '_tile'], 'png');
    close all
    
    clearvars x xtick
    
    %Create variables for Bar plot
    pcorrect=[];
    ttosafe=[];
    timeinsafe=[];
    disttosafe=[];
    
    for i=1:size(trial,3)
        pcorrect=[pcorrect [nanmean(trial(:,11,i)); std(trial(:,11,i))/sqrt(length(trial(:,11,i)))]];
        ttosafe=[ttosafe [nanmean(trial(:,4,i)); std(trial(:,4,i))/sqrt(length(trial(:,4,i)))]];
        timeinsafe=[timeinsafe [nanmean(trial(:,6,i)); std(trial(:,6,i))/sqrt(length(trial(:,6,i)))]];
        disttosafe=[disttosafe [nanmean(trial(:,5,i)); std(trial(:,5,i))/sqrt(length(trial(:,5,i)))]];
    end
    
    %Bar graph proportion correct
    for i=1:size(trial,3);
        bar(i,pcorrect(1,i),'FaceColor',c(i,:));
        hold on
        errorbar(i,pcorrect(1,i),pcorrect(2,i),'color','k','LineWidth',1)
        hold on
    end
    %Graph settings
    title(['Proportion of flies that reached the ' tsuffix{tcnt} ' tile']);
    xlim([0, size(trial,3)+1]);
    ylim([0, 1]);
    set(gca,'XTick',1:length(l),'XTickLabel',l)
    ylabel('Percentage');
    saveas(gcf, ['Proportion_of_flies_that_reached_' tsuffix{tcnt} '_tile'], 'png');
    close all
    
    %Bar graph time to safe
    for i=1:size(trial,3);
        bar(i,ttosafe(1,i),'FaceColor',c(i,:));
        hold on
        errorbar(i,ttosafe(1,i),ttosafe(2,i),'color','k','LineWidth',1)
        hold on
    end
    %Graph settings
    title(['Average time to reach the ' tsuffix{tcnt} ' tile']);
    xlim([0, size(trial,3)+1]);
    set(gca,'XTick',1:length(l),'XTickLabel',l)
    ylabel('Time (seconds)');
    saveas(gcf, ['Average_time_to_reach_' tsuffix{tcnt} '_tile'], 'png');
    close all
    
    %Bar graph total time in safe tile
    for i=1:size(trial,3);
        bar(i,timeinsafe(1,i),'FaceColor',c(i,:));
        hold on
        errorbar(i,timeinsafe(1,i),timeinsafe(2,i),'color','k','LineWidth',1)
        hold on
    end
    %Graph settings
    title(['Average time spent in the ' tsuffix{tcnt} ' tile']);
    xlim([0, size(trial,3)+1]);
    set(gca,'XTick',1:length(l),'XTickLabel',l)
    ylabel('Time (seconds)');
    saveas(gcf, ['Average_time_spent_in_' tsuffix{tcnt} '_tile'], 'png');
    close all
    
    %Bar graph distance travaled to safe tile
    for i=1:size(trial,3);
        bar(i,disttosafe(1,i),'FaceColor',c(i,:));
        hold on
        errorbar(i,disttosafe(1,i),disttosafe(2,i),'color','k','LineWidth',1)
        hold on
    end
    %Graph settings
    title(['Distance traveled to reach the ' tsuffix{tcnt} ' tile']);
    xlim([0, size(trial,3)+1]);
    set(gca,'XTick',1:length(l),'XTickLabel',l)
    ylabel('Distance (pixels)');
    saveas(gcf, ['Distance_traveled_to_reach_' tsuffix{tcnt} '_tile'], 'png');
    close all
    
    %Pole plots
    x=1:size(pole,1);
    
    %Define labels for x-ticks
    for i=1:length(x)
        xtick{i}=num2str(x(i));
    end
    xtick=[' ', xtick, ' '];
    
    %%%% Add Pole plots here
    
    clearvars x xtick
end

%Play sound to allert program has ended
load handel;
sound(y(2000:17500),Fs);

clear all