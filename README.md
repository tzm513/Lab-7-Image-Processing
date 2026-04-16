# Lab-7-Image-Processing

DFT folder:

    Compile & run command:

        gfortran consts.f90 DFT.f90 -o DFT.exe -O3 -std=f2008; ./DFT.exe; python plot.py
    
    Generates a wave based on the parameters provided in params.txt

    Performs a fourier transform on the generated wave

    Plots both the generated waveform (including any windowing) and the fourier transform

Sobel_Edge folder:

    compile & run command:

        gfortran consts.f90 pgm_processing.f90 main.f90 -O3 -std=f2008 -o Sobel.exe; ./Sobel.exe

    Generates image clown_grad.pgm to visuale the edges of clown.pgm, using a Sobel filter