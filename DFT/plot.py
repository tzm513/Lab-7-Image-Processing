import numpy as np
import matplotlib.pyplot as plt

freqs = np.loadtxt("output_freqs.txt", unpack = True)
wave = np.loadtxt("output_wave.txt", unpack = True)

plt.subplot(121)
plt.title("Fourier Transform")
plt.plot(freqs[0], freqs[1])
plt.xlabel("Frequency (Hz)")
plt.xlabel("Magnitude")
plt.xlim(0, int(max(freqs[0])/2))

plt.subplot(122)
plt.title("Waveform")
plt.plot(wave[0], wave[1])
plt.xlabel("Time (s)")
plt.ylabel("Amplitude")
plt.show()