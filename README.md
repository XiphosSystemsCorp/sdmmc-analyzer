# sdmmc-analyzer

Analyze SD/MMC transactions captured using e.g. Saelae Logic's logic analyzers.

## Usage

Connect the SD breakout board to the analyzer using the following pinout:

Wire   | Bit   | Signal  |
-------|-------|---------|
Black  | Bit 0 | Dat0    |
Brown  | Bit 1 | Dat1    |
Red    | Bit 2 | Dat2    |
Orange | Bit 3 | Dat3/CD |
Yellow | Bit 4 | Cmd     |
Green  | Bit 5 | Clk     |

Trigger on cmd falling.

Export data in binary.

Analyzer with: sdmmc-analyzer data.bin

## Compilation

    ./configure
    make
    make install

## License

Copyright (C)2011-2014 Xiphos Systems Corposation
Author: Berke Durak <obd@xiphos.com>
