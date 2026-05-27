// 20/10/2025 - LAB EXPERIMENT | WATER FLOW THROUGH LIVE POLE DRAINS
// Author: Fernanda Berlitz 
// E-mail: fernanda.berlitz@gcu.ac.uk


// TASK 09: CLASSIFY HUE WITH DIFFERENT THRESHOLDS FOR SENSITIVITY ANALYSIS

// 1. Get Directories
input = getDirectory("Select input folder where images are stored"); // data/input/cross-section_photos
master_output = getDirectory("Select master output folder for results"); //data/output/hue_class


// Ensure master_output ends with the correct OS slash
if (!endsWith(master_output, File.separator)) {
    master_output = master_output + File.separator;
}

// 2. Automatically create subfolders to keep things clean
out100 = master_output + "OUTPUT_TH100" + File.separator;
out125 = master_output + "OUTPUT_TH125" + File.separator;

// Safely create folders only if they don't already exist
if (!File.exists(out100)) { File.makeDirectory(out100); }
if (!File.exists(out125)) { File.makeDirectory(out125); }

list = getFileList(input);
setBatchMode(true); // hide image outputs while running
run("Set Measurements...", "area mean standard centroid median skewness limit invert redirect=None decimal=4");

for (i = 0; i < list.length; i++) {
    // Open image and set scale
    open(input + list[i]);
    run("Set Scale...", "distance=238 known=10 unit=mm global");

    // Get file name and duplicate
    slice = File.nameWithoutExtension;
    rename("OR_slice.png");
    run("Duplicate...", "title=Processing_Slice");

    // Create Hue and Saturation images
    run("HSB Stack");
    run("Stack to Images");

    // Close unnecessary images
    selectImage("Saturation");
    close();
    selectImage("Brightness");
    close();


    selectImage("Hue");


    // Add median filter to remove noise
    run("Duplicate...", "title=Hue_filter.png");
    selectImage("Hue_filter.png");
    run("Median...", "radius=1");


    // ==========================================
    // ANALYSIS 1: THRESHOLD 100
    // ==========================================
    selectImage("Hue_filter.png");
    resetThreshold;
    setThreshold(100, 255, "raw");
    
    run("Create Mask");
    saveAs("PNG", out100 + i + "_" + slice + "_LAT_OPEN_TH100.png");
    
    // Create selection from mask
    setAutoThreshold("Default dark no-reset");
    setThreshold(255, 255, "raw");
    run("Create Selection");
    roiManager("Add");
    run("Select None");
    resetThreshold;
    close(); // Closes the TH100 mask image

    // Measure and Save
    roiManager("Measure");
    saveAs("Results", out100 + i + "_" + slice + "_Results_TH100.csv");
    run("Clear Results");
    roiManager("Save", out100 + i + "_" + slice + "_RoiSet_TH100.zip");
    roiManager("Delete");


    // ==========================================
    // ANALYSIS 2: THRESHOLD 125
    // ==========================================
    selectImage("Hue_filter.png");
    resetThreshold;
    setThreshold(125, 255, "raw");
    
    run("Create Mask");
    saveAs("PNG", out125 + i + "_" + slice + "_LAT_OPEN_TH125.png");
    
    // Create selection from mask
    setAutoThreshold("Default dark no-reset");
    setThreshold(255, 255, "raw");
    run("Create Selection");
    roiManager("Add");
    run("Select None");
    resetThreshold;
    close(); // Closes the TH125 mask image

    // Measure and Save
    roiManager("Measure");
    saveAs("Results", out125 + i + "_" + slice + "_Results_TH125.csv");
    run("Clear Results");
    roiManager("Save", out125 + i + "_" + slice + "_RoiSet_TH125.zip");
    roiManager("Delete");


    // Close all remaining windows before opening the next image in the list
    run("Close All");
}

setBatchMode(false); // Restore normal ImageJ behavior
print("Batch processing complete! Check your master folder for results.");
