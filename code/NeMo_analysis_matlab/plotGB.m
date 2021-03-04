%% plot glass brains
tmp = load(ChaCoResultsFilename);

log1x3 = cellfun(@(p)(contains(tmp.subjects,p)),{'1-14-006','2-01-070', '5-04-010'}, 'uni', false);
idx = ~(log1x3{1} | log1x3{2} | log1x3{3});

ChaCoResults.Regions = mean(abs(tmp.CD.mean(idx,:)));
ChaCoResultsFilenameTemp = [outdir filesep '..' filesep './ChaCoResultsFileNameTemp.mat'];
save(ChaCoResultsFilenameTemp,'ChaCoResults');
GBPlot.flag = 1;
GBPlot.movie = false;
SurfPlot.flag = 0;
BoxPlot.flag = 0;
GraphPlot.flag = 0;
figstr = ['figure_' visit '_' num2str(atlassize)];
plotlobecolor = true;

disp('run PlotChaCoResults ...')

PlotChaCoResults_col(ChaCoResultsFilenameTemp, GBPlot, SurfPlot, BoxPlot, GraphPlot, figstr, plotlobecolor)

delete(ChaCoResultsFilenameTemp)
