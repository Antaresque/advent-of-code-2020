let isBetween = (n, a, b) => (n >= a && n <= b);

function part1(input) {
    let valid = 0;

    for(let obj of input) {
        let counter = 0;
        for(let c of obj.password) {
            if(c == obj.char)
                counter++;
        }
        if(isBetween(counter, obj.min, obj.max))
            valid++;
    }

    console.log(`VALID: %d`, valid);
}

function part2(input) {
    let valid = 0;

    for(let obj of input) {
        let charA = obj.password.charAt(obj.min - 1);
        let charB = obj.password.charAt(obj.max - 1);
        if(charA == charB)
            continue;
        if(charA == obj.char || charB == obj.char)
            valid++;
    }

    console.log(`VALID: %d`, valid);
}


const input = require('fs').readFileSync('./data.txt')
    .toString()
    .split("\r\n")  
    .map(n => n.split(" "))     // ["a-b", "c:", "pass"]
    .map(arr => {
        let obj = {};
        let interval = arr[0].split("-");
        obj.min = parseInt(interval[0]);
        obj.max = parseInt(interval[1]);
        obj.char = arr[1].charAt(0);
        obj.password = arr[2];
        return obj;
    });

part1(input);
part2(input);
