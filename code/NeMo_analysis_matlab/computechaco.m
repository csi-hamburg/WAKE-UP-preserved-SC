
if(~exist('CHUNKS','var')); CHUNKS=1; end
if(~exist('MAGICNUMBER','var')); MAGICNUMBER=0; end

if(~exist('SORT_MODE','var'));
       	idx = 1:n;
else
	[~, idx] = sort(vol, SORT_MODE);
end

idx = idx(mod(1:n,CHUNKS)==MAGICNUMBER);

timings = nan(n,1);
args = cell(numel(idx),1);


func = @(vol,DamageFileName,Coreg2MNI,CalcSpace,atlassize,StrSave,NumWorkers,dispMask,coregOnly,main_dir)(ChaCoCalc(DamageFileName,Coreg2MNI,CalcSpace,atlassize,StrSave,NumWorkers,dispMask,coregOnly,main_dir));

for i = 1:numel(idx)
    subject = subjects{idx(i)};
    sprintf('Processing file %s (%d/%d)\n',subject,i,n)
    DamageFileName = [lesiondir filesep subject '.nii'];
    StrSave = [outdir filesep subject];

    Coreg2MNI = struct('StructImageType',{},'ImageFileName',{});
    CalcSpace = 'MNI';
    NumWorkers = 7;
    dispMask = true;
    coregOnly = false;
    
    if ~exist(StrSave,'dir'); mkdir(StrSave); end
    args{i} = {vol(idx(i)), DamageFileName,Coreg2MNI,CalcSpace,atlassize,StrSave,NumWorkers,dispMask,coregOnly,main_dir};
    func(args{i}{:})
end
