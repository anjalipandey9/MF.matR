filepath1 = File.openDialog("Select a File");
open(filepath1);
dir = File.getParent(filepath1);
fname = File.nameWithoutExtension();
fname2 = replace(fname,"_MMStack_Pos0.ome","")
waitForUser("REGISTRATION: select large square around neuron (just click 'OK' on the next 2 popups)");
setTool("rectangle");
run("Align slices in stack...");
run("Enlarge...", "enlarge=60");
//run("Crop");
saveAs("Tiff", dir + "/" + fname2 + "_reg");
//close();
run("In [+]");
run("In [+]");
setTool("freehand");
waitForUser("GCaMP: select ROI around neuron");
run("Set Measurements...", "area mean integrated modal min median stack display invert redirect=None decimal=3");
//saveSettings;
setOption("Stack position", true);
       for (n=1; n<=nSlices; n++) {
          setSlice(n);
          run("Measure");
      }
//restoreSettings;
saveAs("Results", dir + "/" + fname2 + "_neuron_results.csv");
run("Clear Results");
run("Restore Selection");
setTool("freehand");
waitForUser("move ROI to select background ROI");
saveSettings;
setOption("Stack position", true);
       for (n=1; n<=nSlices; n++) {
          setSlice(n);
          run("Measure");
      }
restoreSettings;
saveAs("Results", dir + "/" + fname2 + "_background_results.csv");
run("Clear Results");
close();