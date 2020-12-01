function part1(input) {
    for(let i = 0; i < input.length; i++) {
        for(let j = i + 1; j < input.length; j++) {
            let el1 = input[i];
            let el2 = input[j];

            if(el1 + el2 == 2020) 
                console.log(`${el1}, ${el2}, ${el1 * el2}`);
        }
    }
}

function part2(input) {
    for(let i = 0; i < input.length; i++) {
        for(let j = i + 1; j < input.length; j++) {
            for(let k = j + 1; k < input.length; k++) {
                let el1 = input[i];
                let el2 = input[j];
                let el3 = input[k];

                if(el1 + el2 + el3 == 2020) 
                    console.log(`${el1}, ${el2}, ${el3}, ${el1 * el2 * el3}`);
                
            }
        }
    }
}

const input = require('fs').readFileSync('./data.txt')
    .toString()
    .split("\r\n")
    .map(n => parseInt(n));
part1(input);
part2(input);