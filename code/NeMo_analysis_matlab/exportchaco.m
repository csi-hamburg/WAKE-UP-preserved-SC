

NeMoanalysisdir = fileparts(which('computechaco.m')); 
basedir = [NeMoanalysisdir filesep '..' filesep '..'];
outdir = [basedir filesep 'derivatives' filesep 'NeMo_output'];

V0 = load([outdir filesep 'V0' filesep num2str(atlassize) filesep 'ChaCo.mat']);
V3 = load([outdir filesep 'V3' filesep num2str(atlassize) filesep 'ChaCo.mat']);



V0.NeMo=reshape(V0.CD.mean,[],1);
V3.NeMo=reshape(V3.CD.mean,[],1);


fid = fopen([basedir filesep 'derivatives' filesep 'subjectsV0V3PACS.dat'], 'r');
data = textscan(fid, '%s');
fclose(fid);
subjectsID = data{1};
clear data

fid=fopen([basedir filesep 'clinical' filesep 'v3names.csv']);
data=textscan(fid,'%s%s%s%f%f%s%d','Delimiter',',','Headerlines',1);
fclose(fid);
idx=cellfun(@(s)(find(1-cellfun(@isempty,strfind(data{1},s)))),subjectsID);

numel(idx)


ll=cellfun(@(l)(repmat({l},[numel(subjectsID),1])),V0.CD.labels,'uni',false);

if(atlassize == 86)
    load(['resource' filesep 'Convert_86to7atlas.mat']); % lobe allocation for FS86 atlas
    lobes = arrayfun(@(l)(repmat({l},[numel(subjectsID),1])),Functional86_roi,'uni',false);
elseif(atlassize == 116)
    lobes = arrayfun(@(l)(repmat({l},[numel(subjectsID),1])),zeros(atlassize,1),'uni',false);
end

c=[V0.NeMo, V3.NeMo, {reshape([ll{:}],[],1)}, cellfun(@(c)(repmat(c(idx),[atlassize,1])),data,'uni',false)];

rownames={'nemoscoreV0','nemoscoreV3','lab','ID','treatment','lesion_side','lesionvolumeV0','lesionvolumeV3','lesion_location','lesion_supratentorial', 'lobe'};

cNUM = c([1 2 7 8 10]);
cNUM = cellfun(@double,cNUM,'uni',false);
tabNUM = array2table([cNUM{:}],'VariableNames',rownames([1 2 7 8 10]));
cTXT = c([3 4 5 6 9]);
tabTXT = cell2table([cTXT{:}],'VariableNames',rownames([3 4 5 6 9]));

tab=[tabTXT, tabNUM];

writetable(tab,[outdir filesep 'nemo' num2str(atlassize) '.csv'])
