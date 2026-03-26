# Lab-8-Image-Processing

DFT folder:
    Compile & run command:
        gfortran consts.f90 DFT.f90 -o DFT.exe -O3 -std=f2008; ./DFT.exe; python plot.py
    
    Generates a wave based on the parameters provided in params.txt
    Performs a fourier transform on the generated wave
    Plots both the generated waveform (including any windowing) and the fourier transform