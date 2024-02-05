function solve(target, numbers) {
    let ops = ['+', '-', '*', '/'];

}

function subdivide(numbers) {
    let len = numbers.length;
    let num = Math.pow(2, len);
    let result = [];
    for (let i = 1; i < num - 1; i++) {
        result.push(bitsplit(numbers, i))
    }
    return result;
}

function bitsplit(l, n) {
    let left = [];
    let right = [];
    for (let i = 0; i < l.length; i++) {
        if (n % 2 == 0) {
            left.push(l[i]);
        } else {
            right.push(l[i]);
        }
        n = Math.floor(n / 2);
    }
    return [left, right];
}
