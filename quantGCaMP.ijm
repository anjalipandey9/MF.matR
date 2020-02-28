filepath1 = File.openDialog("Select a File");
open(filepath1);
dir = File.getParent(filepath1);
fname = File.nameWithoutExtension();
waitForUser("GCaMP: select ROI around neuron");
run("Set Measurements...", "area mean modal min median stack display invert redirect=None decimal=3");
saveSettings;
setTool("freehand");
setOption("Stack position", true);
       for (n=1; n<=nSlices; n++) {
          setSlice(n);
          run("Measure");
      }
restoreSettings;
saveAs("Results", dir + "/" + fname + "_neuron_results.csv");
run("Clear Results");
waitForUser("select background ROI");
saveSettings;
setOption("Stack position", true);
       for (n=1; n<=nSlices; n++) {
          setSlice(n);
          run("Measure");
      }
restoreSettings;
saveAs("Results", dir + "/" + fname + "_background_results.csv");
run("Clear Results");
close();