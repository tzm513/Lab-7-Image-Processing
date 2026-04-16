# Lab-7-Image-Processing

DFT folder:

    Compile & run command:

        gfortran consts.f90 DFT.f90 -o DFT.exe -O3 -std=f2008; ./DFT.exe; python plot.py
    
    Generates a wave based on the parameters provided in params.txt

    Performs a fourier transform on the generated wave

    Plots both the generated waveform (including any windowing) and the fourier transform

Sobel_Edge folder:

    Compile & run command:

        gfortran consts.f90 pgm_processing.f90 main.f90 -O3 -std=f2008 -o Sobel.exe; ./Sobel.exe

    Generates image clown_grad.pgm to visuale the edges of clown.pgm, using a Sobel filter

FFT_Edge folder:

    Compile & run command:

        gfortran consts.f90 pgm_processing.f90 FFTW_Setup.f90 fft.f90 -lfftw3 -O3 -std=f2008 -o fft.exe
        ; ./fft.exe
    
    Performs an edge detection using the FFTW3 library

Deconvolution folder:

    Compile & run command:

        gfortran consts.f90 pgm_processing.f90 FFTW_Setup.f90 deconv.f90.f90 -lfftw3 -O3 -std=f2008 -o deconv.exe
        ; ./deconv.exe
    
    Removes blur from the provided image using the FFTW3 library