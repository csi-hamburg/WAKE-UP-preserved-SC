% -extract a list of subjects with anterior-circulation stroke and imaging available from T1 and T2
% -extract their lesion volumes


NeMoanalysisdir = fileparts(which('computechaco.m'));
basedir = [NeMoanalysisdir filesep '..' filesep '..'];

lesiondir = [basedir filesep 'lesionmasks' filesep 'derivatives'];

for visit = {'V0','V3'}

	visit = visit{1};

	subjects.(visit) = dir([lesiondir filesep visit]);
	subjects.V0 = subjects.V0(3:end); % omit '.' and '..' entries

	% strip file ending .nii.gz
	[~, SN, ~] = cellfun(@fileparts,{subjects.(visit).name}, 'uni', false);
	[subjects.(visit).name] = SN{:};
	[~, SN, ~] = cellfun(@fileparts,{subjects.(visit).name}, 'uni', false);
	[subjects.(visit).name] = SN{:};

	switch visit
	case 'V0'
		tail = 28;
	case 'V3'
		tail = 19;
	otherwise
		error('visit misspecified');
	end

	subjectsID.(visit) = cellfun(@(s)(s(1:end-tail)),{subjects.(visit).name}, 'uni',false);

end




subjectsID.V0V3 = intersect(subjectsID.V0,subjectsID.V3);
fid = fopen([basedir filesep 'derivatives'  filesep 'subjectsV0V3all.dat'], 'w');
fprintf(fid, '%s\n', subjectsID.V0V3{:});
fclose(fid);

idx.V0 = cellfun(@(s)(find(1-cellfun(@isempty,strfind(subjectsID.(visit),s)))),subjectsID.V0V3);
idx.V3 = cellfun(@(s)(find(1-cellfun(@isempty,strfind(subjectsID.(visit),s)))),subjectsID.V0V3);



fid = fopen([basedir filesep 'clinical' filesep 'v3names.csv']);
data = textscan(fid,'%s%s%s%f%f%s%d','Delimiter',',','Headerlines',1);
fclose(fid);
idx.V0V3 = cellfun(@(s)(find(1-cellfun(@isempty,strfind(data{1},s)))),subjectsID.V0V3);


bamford = data{6}(idx.V0V3);
side = data{3}(idx.V0V3);

idx.V0V3_PACS = cellfun(@(b)(contains(b,'mcaOrAca')),bamford) & cellfun(@(b)(~contains(b,'both')),side);

subjectsID.V0V3_PACS = subjectsID.V0V3(idx.V0V3_PACS);
fid = fopen([basedir filesep 'derivatives' filesep 'subjectsV0V3PACS.dat'], 'w');
fprintf(fid, '%s\n', subjectsID.V0V3_PACS{:});
fclose(fid);


vol.V3 = [data{4}, data{5}];
vol.V0V3_PACS = vol.V3(idx.V0V3(idx.V0V3_PACS),:);
csvwrite([basedir filesep 'derivatives' filesep 'volV0V3PACS.dat'], vol.V0V3_PACS);

clear data bamford side idx
