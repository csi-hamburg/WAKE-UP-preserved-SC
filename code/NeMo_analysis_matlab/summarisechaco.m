CD=struct();
CD.raw = nan(n,nTract,atlassize);
CD.mean = nan(n,atlassize);
CD.median = nan(n,atlassize);
CD.sd = nan(n,atlassize);
CD.labels = cell(1,atlassize);

for i = 1:n
    subject = subjects{i};
    sprintf('Processing file %s (%d/%d)\n',subject,i,n)    
    StrSave = [outdir filesep subject];
    data=load([StrSave filesep 'ChaCo' num2str(atlassize) '_MNI.mat']);
    
    temp=struct2table(data.ChaCoResults);
    CD.raw(i,:,:) = temp.Regions(2:end,:); % raw ChaCo
    CD.mean(i,:) = nanmean(temp.Regions(2:end,:),1); % mean ChaCo
    CD.median(i,:) = nanmedian(temp.Regions(2:end,:),1); % median ChaCo
    CD.sd(i,:) = nanstd(temp.Regions(2:end,:),1); % sd ChaCo
end

fid=fopen([start_dir filesep '..' filesep 'resource' filesep 'atlas'  num2str(atlassize) '.cod']);
data=textscan(fid,'%s');
fclose(fid);

CD.labels = data{1};

%%
save(ChaCoResultsFilename,'subjects','CD');
