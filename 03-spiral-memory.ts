// tslint:disable:quotemark
// tslint:disable:typedef
// tslint:disable:comment-format

// Part 1

// square width
// step 0: 1
// step n: n*2 + 1

// number of items in the square:
// step 0: 1
// step n: (w * 2) + ((w - 2) * 2) = 2w + 2w - 4 = 4w - 4 = 4(n*2 + 1) - 4 = 8n + 4 - 4 = 8n

// 8 + 2*8 + 3*8 + 4*8 = 8 * (1 + 2 + 3 + 4)
// 8 * (1 + 2 + 3 + 4...si) < i

const part1 = () => {
    const input = 265149;

    const numbersInSquare = (iter: number) => iter === 0 ? 1 : iter * 8;

    let sum = 0;
    for (let i = 0; i < 1000; i++) {
        const count = numbersInSquare(i);
        sum += count;

        if (sum >= input) {
            const relative = input - (sum - count);
            const width = i * 2 + 1;
            const mod = relative % (width - 1);
            const centerLine = (width - 1) / 2;
            const fromCenterLine = Math.abs(mod - centerLine);

            const result = i + fromCenterLine;

            console.log(`Iteration with the number ${input} is: #${i}`);
            console.log(`Sum so far: ${sum}`);
            console.log(`Number of items in this square: ${count}`);
            console.log(`It is the ${relative}th number in the square.`);
            console.log(`The width of the square: ${width}`);
            console.log(`Mod: ${mod}`);
            console.log(`CenterLine: ${centerLine}`);
            console.log(`FromCenterLine: ${fromCenterLine}`);

            console.log(`Steps needed: ${result}`);
            break;
        }
    }
};

// Part 2
const part2 = () => {
    const input = 25;

    const numbersInSquare = (iter: number) => iter === 0 ? 1 : iter * 8;

    const indexToCoords = (index: number, squareIndex: number, sum: number) => {
        const count = numbersInSquare(squareIndex);
        const relative = index + 1 - (sum - count);
        const width = squareIndex * 2 + 1;
        const mod = relative % (width - 1);
        const div = Math.floor((relative - 1) / (width - 1));
        const halfWidth = (width - 1) / 2;

        return [
            div === 0 ? halfWidth :
            div === 2 ? -1 * halfWidth :
            div === 1 ? (mod === 0 ? -1 * halfWidth : halfWidth - mod) :
                (mod === 0 ? halfWidth : -1 * ((width - 1) / 2 - mod)),
            div === 1 ? halfWidth :
            div === 3 ? -1 * halfWidth :
            div === 2 ? (mod === 0 ? -1 * halfWidth : halfWidth - mod) :
                (mod === 0 ? halfWidth : -1 * ((width - 1) / 2 - mod)),
        ];
    };

    const numbers: number[][] = [[1]];

    const saveNumber = (coords: number[], number: number) => {
        if(!numbers[coords[0]]) {
            numbers[coords[0]] = [];
        }

        numbers[coords[0]][coords[1]] = number;
    };

    const getNumber = (coords: number[]) => {
        if(!numbers[coords[0]]) {
            return undefined;
        }

        return numbers[coords[0]][coords[1]];
    };

    let squareIndex = 1;
    let indexSum = 1;
    let sum = 9;
    for (let i = 1; i < 10000; i++) {
        if(i >= indexSum + 8 ) {
            squareIndex++;
            indexSum += 8 * squareIndex;
            sum += numbersInSquare(squareIndex);
        }

        const coords = indexToCoords(i, squareIndex, sum);
        const [x, y] = coords;

        let newNum = 0;
        newNum += getNumber([x + 1, y]) || 0;
        newNum += getNumber([x + 1, y + 1]) || 0;
        newNum += getNumber([x, y + 1]) || 0;
        newNum += getNumber([x - 1, y + 1]) || 0;
        newNum += getNumber([x - 1, y]) || 0;
        newNum += getNumber([x - 1, y - 1]) || 0;
        newNum += getNumber([x, y - 1]) || 0;
        newNum += getNumber([x + 1, y - 1]) || 0;

        saveNumber(coords, newNum);

        if(newNum > 265149) {
            console.log(`The result is ${newNum}`);
            break;
        }
    }
};

part2();
