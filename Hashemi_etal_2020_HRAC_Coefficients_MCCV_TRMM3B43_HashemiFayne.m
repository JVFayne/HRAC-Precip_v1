clear all
close all

%navigate to the folder containing the yearly input data
%(/HRAC_MCCV_CoefficientAnalysisData/)
mat = dir('*.mat'); for q = 1:length(mat) load(mat(q).name); end
%this will read in all of the input data into one data frame

DATA=transpose({Data1998,Data1999,Data2000,Data2001,Data2002,Data2003,Data2004,Data2005,Data2006,Data2007,Data2008,Data2009,Data2010,Data2011,Data2012,Data2013,Data2014});
%%
n_times = 1000;
 
CC = zeros(12,n_times);
DD = zeros(12,n_times);
 
ORIGINAL_MAE = zeros(12,n_times);
CORRECTED_MAE = zeros(12,n_times);
 
ORIGINAL_RMSE = zeros(12,n_times);
CORRECTED_RMSE = zeros(12,n_times);
 
TOTAL_PRECIPITATION_P = zeros(12,n_times);
TOTAL_PRECIPITATION_T = zeros(12,n_times);
TOTAL_PRECIPITATION_Tc = zeros(12,n_times);
 
for times = 1:n_times
 
delta = 1; % truncation near 0 precipitation
 
% indices to validation years
cal_inds = ceil(rand(1,4)*17);
 
ncal = length(cal_inds);
 
val_inds = setdiff(1:17,cal_inds);
nval = length(val_inds);
 
alpha = zeros(1,12); % alpha and beta are the coefficients seen in Table 2 
beta = zeros(1,12); % due to the iterative nature of the MCCV, 
%these coefficients will change slightly with every model run. 
%The results do not change significantly, but we recommend using the
%published coefficients as those are the ones that have beenn validated in
%the SciData manuscript
 
% get indices to high elevation
DEM = DATA{1}.DEM;
inds_high = find(DEM>1500);
E = DEM(inds_high);    
    
month = 1;
year = 1;
 
for month =1:12
    
        % average over calibration years
        T = zeros(size(E));
        P = zeros(size(E));
        for n=1:ncal
            year = cal_inds(n);
            % get precipitation gg
            PRISM = DATA{year}.PRISM{month};
            TRMM = DATA{year}.TRMM{month};                    
            T = T + TRMM(inds_high);
            P = P + PRISM(inds_high);
        end
        T = T/ncal;
        P = P/ncal;
        
%         B = (T-P)./(T+P+delta)*2;
%         x = polyfit(E,B,1);        
        x = polyfit(E,P./(T+1)-1,1);
        alpha(month) = x(1);
        beta(month) = x(2);
end
 
% run validation period
 
DEM = DATA{1}.DEM;
 
% one location
% inds_high = sub2ind(size(DEM),82,65);
 
% all locations
inds_high = find(DEM>1500);
 
Pt = zeros(1,12);
Tt = zeros(1,12);
Tct = zeros(1,12);
TEMPt = zeros(1,12);
 
% arrange vectors for scatter plots
Ps = cell(1,12);
Ts = cell(1,12);
Tcs = cell(1,12);
for month=1:12
    Ps{month} = zeros(size(inds_high));
    Ts{month} = zeros(size(inds_high));
    Tcs{month} = zeros(size(inds_high));
end
ELs = zeros(size(inds_high));
Els = DEM(inds_high);
 
% arrange correction 
f = @(c,d) Els*c + d + 1;
 
for month=1:12
    for n = 1:nval
        
        year = val_inds(n);
        PRISM = DATA{year}.PRISM{month};
        TRMM = DATA{year}.TRMM{month};
        temp = DATA{year}.TEMP{month};
        temp = temp(inds_high);
        T = TRMM(inds_high);
        P = PRISM(inds_high);
 
        % apply correction
        Tc = T.*f(alpha(month),beta(month));
        
        % collect total precipitation
        Pt(month) = Pt(month) + sum(P);
        Tt(month) = Tt(month) + sum(T);
        Tct(month) = Tct(month) + sum(Tc);
        TEMPt(month) = TEMPt(month) +  sum(temp);
        
        % collect scatter plots
        Ps{month} = Ps{month} + P;
        Ts{month} = Ts{month} + T;
        Tcs{month} = Tcs{month} + Tc;
        
    end
end
 
% calculate rmse
original_rmse = zeros(1,12);
corrected_rmse = zeros(1,12);

% calculate mean absolute error (less sensitive to occasional large errors) 
original_mae = zeros(1,12);
corrected_mae = zeros(1,12);
 
original_mape = zeros(1,12);
corrected_mape = zeros(1,12);
 
for month=1:12
    original_rmse(month) = sqrt(sum((Ps{month}/nval-Ts{month}/nval).^2));
    corrected_rmse(month) = sqrt(sum((Ps{month}/nval-Tcs{month}/nval).^2));
    
    original_mae(month) = sum(abs((Ps{month}/nval-Ts{month}/nval)));
    corrected_mae(month) = sum(abs((Ps{month}/nval-Tcs{month}/nval)));
    
    original_mape(month) = sum(abs(((Ps{month}/nval-Ts{month}/nval)./(Ps{month}/nval))));
    corrected_mape(month) = sum(abs(((Ps{month}/nval-Tcs{month}/nval)./(Ps{month}/nval))));
    
end
 
% collect parameters
CC(:,times) = alpha(:);
DD(:,times) = beta(:);
 
ORIGINAL_MAE(:,times) = original_mae(:);
CORRECTED_MAE(:,times) = corrected_mae(:);
 
ORIGINAL_RMSE(:,times) = original_rmse(:);
CORRECTED_RMSE(:,times) = corrected_rmse(:);
 
TOTAL_PRECIPITATION_P(:,times) = Pt(:);
TOTAL_PRECIPITATION_T(:,times) = Tt(:);
TOTAL_PRECIPITATION_Tc(:,times) = Tct(:);
end
 
ORIGINAL_MAE = ORIGINAL_MAE/numel(inds_high);
CORRECTED_MAE = CORRECTED_MAE/numel(inds_high);
 
ORIGINAL_RMSE = ORIGINAL_RMSE/numel(inds_high);
CORRECTED_RMSE = CORRECTED_RMSE/numel(inds_high);
