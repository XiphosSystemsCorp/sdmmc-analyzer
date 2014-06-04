# sdmmc-analyzer

Analyze SD/MMC transactions captured using e.g. Saelae Logic's logic analyzers.

## Usage

* Connect the SD breakout board to the analyzer using the following pinout:

Wire   | Bit   | Signal  |
-------|-------|---------|
Black  | Bit 0 | Dat0    |
Brown  | Bit 1 | Dat1    |
Red    | Bit 2 | Dat2    |
Orange | Bit 3 | Dat3/CD |
Yellow | Bit 4 | Cmd     |
Green  | Bit 5 | Clk     |

* Trigger on CMD falling.
* Export data in binary (output data for each sample, output each sample in
8-bit byte)
* Analyze with: sdmmc-analyzer data.bin

NOTE: You will probably need to reduce the SD/MMC controller clock as the Saelae
analyzer can only capture up to 24 MHz on good days.

## Compilation

    ./configure
    make
    make install

## License

Copyright (C)2011-2014 Xiphos Systems Corposation
Author: Berke Durak <obd@xiphos.com>
