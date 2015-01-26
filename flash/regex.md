
1. thy文件中，去除常数N作为参数，使用正则表达式替换
```js
/nat\s+\\<Rightarrow>\s+nat\s+\\<Rightarrow>\s+rule([\w" \n\r\t\[\]:]*)\s+N\s+/
/nat \\<Rightarrow> rule\1 /
```

2. thy文件中，去掉规则表中的常数N
```js
/nat\s+\\<Rightarrow>\s+rule([\w" \n\r\t\[\]:]*)\s+N\s+/
/rule\1 /
```

3. thy文件中，注释两个以上参数的规则表中的规则
```js
/=\s*(\w*)\s+N(.{0,10})\)/
/=\1 \2\)/
```
