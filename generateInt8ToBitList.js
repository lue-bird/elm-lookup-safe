function dec2bin(dec) {
    return (dec >>> 0).toString(2)
}

for (i = 0; i <= 254; ++i) {
    console.log(`    ${i} -> [ ${Array.from(dec2bin(i)).map(digit => digit === "0" ? "Bit.O" : "Bit.I")} ]`)
}
