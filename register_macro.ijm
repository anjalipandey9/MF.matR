filepath1 = File.openDialog("Select a File");
open(filepath1);
dir = File.getParent(filepath1);
fname = File.nameWithoutExtension();
fname2 = replace(fname,"_MMStack_Pos0.ome","")
waitForUser("select square qround neuron");
//setTool("rectangle");
run("Align slices in stack...");
run("Enlarge...", "enlarge=60");
run("Crop");
saveAs("Tiff", dir + "/" + fname2 + "_reg");
close();