// tslint:disable:quotemark
// tslint:disable:typedef
const banks = [4, 1, 15, 12, 0, 9, 9, 5, 5, 8, 7, 3, 14, 5, 12, 3];
const cache = {};

const choose = () => {
    let maxBlocks = 0;
    let index = -1;
    for (let i = 0; i < banks.length; i++) {
        if(banks[i] > maxBlocks) {
            maxBlocks = banks[i];
            index = i;
        }
    }

    return index;
};

const redistribute = (startIndex: number) => {
    let blocksLeft = banks[startIndex];
    banks[startIndex] = 0;

    let index = (startIndex + 1) % banks.length;

    while(blocksLeft > 0) {
        banks[index]++;
        blocksLeft--;
        index = (index + 1) % banks.length;
    }
};

const cacheKey = () => banks.join('#');

let stepCnt = 0;

while(true) {
    cache[cacheKey()] = stepCnt;

    const index = choose();
    redistribute(index);
    stepCnt++;

    if(cache[cacheKey()]) {
        console.log('Number of redistributions: ', stepCnt);
        console.log('Cycle length: ', stepCnt - cache[cacheKey()]);
        break;
    }
}