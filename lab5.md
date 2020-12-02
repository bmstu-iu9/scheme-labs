
<p><b>Лабораторная работа №5</b>
<h1>Интерпретатор стекового языка программирования</h1>

<h2>Условие задачи</h2>
<p>Реализуйте интерпретатор стекового языка программирования, описание которого
представлено ниже. Интерпретатор должен вызываться 
как процедура <i>(interpret program stack)</i> которая принимает программу
на исходном языке <i>program</i> и начальное состояние стека данных
<i>stack</i> и возвращает его состояние после вычисления программы. 
Программа на исходном языке задана вектором литеральных констант, 
соответствующих словам исходного языка. Исходное и конечное состояния стека
данных являются списком, голова которого соответствует вершине стека.

<p>Примеры вызова интерпретатора (здесь и далее в примерах код на 
исходном языке выделен синим цветом):
<pre>
(interpret #(   <span style="color: blue;">define abs 
                  dup 0 &lt; 
                  if neg endif 
                end 
                abs</span>    ) ; программа
           '(-9))        ; исходное состояние стека
  &#8658; (9)
</pre> 

<p>При реализации интерпретатора избегайте императивных конструкций, используйте
модель вычислений без состояний. Для хранения программы и состояния интерпретатора
<b>запрещается</b> использовать глобальные переменные. Перечисленные 
ниже встроенные слова обязательны для реализации и будут
проверены сервером тестирования.

<h2>Описание языка</h2>

<p>Язык, интерпретатор которого следует реализовать,
является видоизмененным ограниченным подмножеством
языка <a href="http://ru.wikipedia.org/wiki/Forth" target="_blank">Forth</a>.

<p>В нашем языке операции осуществляются с целыми числами. 
Используется постфиксная запись операторов. 
Все вычисления осуществляются на стеке данных. Стек данных является глобальным. 
При запуске интерпретатора стек может быть инициализирован
некоторыми исходными данными или быть пустым. 

<p>Программа на исходном языке представляет собой последовательность слов. 
Интерпретатор анализирует слова по очереди. Если слово является целым числом, 
то оно число помещается на вершину стека данных. В противном случае слово 
интерпретируется как оператор (процедура). Если в программе уже встретилось
определение этого слова (статья), то выполняется код этого определения.
В противном случае слово рассматривается как встроенное в интерпретатор и
выполняется соответствующей процедурой интерпретатора. Затем осуществляется
возврат из процедуры (переход к слову, следующему за последним вызовом). 
Выполнение программы заканчивается, когда выполнено последнее слово.

<p>Процедуры (операторы) снимают свои аргументы с вершины стека данных и 
кладут результат вычислений также на вершину стека данных.

<p>Ввод-вывод или какое-либо взаимодействие с пользователем не предусматривается.

<p>Например:
<pre>
(interpret #(<span style="color: blue;">2 3 * 4 5 * +</span>) '()) &#8658; (26)
</pre>

<h2>Встроенные слова</h2>

<p>Ниже представлен список встроенных слов с кратким описанием их значений.
Состояние стека до и после интерпретации каждого слова показаны с помощью
схем &mdash; стековых диаграмм. Порядок, в котором элементы были помещены в стек,
отражен в индексах элементов. Например, программа:
<pre>
<span style="color: blue;">1 2 3</span>
</pre>
может быть показана стековой диаграммой () &#8594; (1 2 3)

<p><small><b>Внимание!</b> В нашем интерпретаторе в качестве стека используется
список. Голова этого списка является вершиной стека, поэтому вершина стека
в этих диаграммах находится <em>слева</em>!
Такая запись отличается от традиционных стековых диаграмм,
принятых, например, в языке Forth, в которых голова стека записывается 
<em>справа</em>.</small>

<p><b>Арифметические операции</b>
<table>
<tr><td style="text-align: left; vertical-align: top;">+        </td><td style="text-align: left; vertical-align: top;">(n2 n1) &#8594; (сумма)             </td><td style="text-align: left; vertical-align: top;">Сумма n1 и n2</td></tr>
<tr><td style="text-align: left; vertical-align: top;">&minus;  </td><td style="text-align: left; vertical-align: top;">(n2 n1) &#8594; (разность)          </td><td style="text-align: left; vertical-align: top;">Разность: n1 &minus; n2</td></tr>
<tr><td style="text-align: left; vertical-align: top;">*        </td><td style="text-align: left; vertical-align: top;">(n2 n1) &#8594; (произведение)&nbsp;</td><td style="text-align: left; vertical-align: top;">Произведение n2 на n1</td></tr>
<tr><td style="text-align: left; vertical-align: top;">/        </td><td style="text-align: left; vertical-align: top;">(n2 n1) &#8594; (частное)           </td><td style="text-align: left; vertical-align: top;">Целочисленное деление n1 на n2</td></tr>
<tr><td style="text-align: left; vertical-align: top;">mod&nbsp;</td><td style="text-align: left; vertical-align: top;">(n2 n1) &#8594; (остаток)           </td><td style="text-align: left; vertical-align: top;">Остаток от деления n1 на n2</td></tr>
<tr><td style="text-align: left; vertical-align: top;">neg      </td><td style="text-align: left; vertical-align: top;">(n) &#8594; (&minus;n)              </td><td style="text-align: left; vertical-align: top;">Смена знака числа</td></tr>
</table>

<p><b>Операции сравнения</b>
<table>
<tr><td style="text-align: left; vertical-align: top;">=&nbsp;</td><td style="text-align: left; vertical-align: top;">(n2 n1) &#8594; (флаг) &nbsp;</td><td style="text-align: left; vertical-align: top;">Флаг равен &minus;1, если n1 = n2, иначе флаг равен 0</td></tr>
<tr><td style="text-align: left; vertical-align: top;">&gt;   </td><td style="text-align: left; vertical-align: top;">(n2 n1) &#8594; (флаг) &nbsp;</td><td style="text-align: left; vertical-align: top;">Флаг равен &minus;1, если n1 &gt; n2, иначе флаг равен 0</td></tr>
<tr><td style="text-align: left; vertical-align: top;">&lt;   </td><td style="text-align: left; vertical-align: top;">(n2 n1) &#8594; (флаг) &nbsp;</td><td style="text-align: left; vertical-align: top;">Флаг равен &minus;1, если n1 &lt; n2, иначе флаг равен 0</td></tr>
</table>
<p>Таким образом, булевы значения представлены с помощью целых чисел:
&minus;1 соответствует значению &laquo;истина&raquo;, 0 &mdash; значению &laquo;ложь&raquo;.

<p><b>Логические операции</b>
<table>
<tr><td style="text-align: left; vertical-align: top;">not&nbsp;</td><td style="text-align: left; vertical-align: top;">(n)     &#8594; (результат)&nbsp;</td><td style="text-align: left; vertical-align: top;">НЕ n</td></tr>
<tr><td style="text-align: left; vertical-align: top;">and&nbsp;</td><td style="text-align: left; vertical-align: top;">(n2 n1) &#8594; (результат)&nbsp;</td><td style="text-align: left; vertical-align: top;">n2 И n1</td></tr>
<tr><td style="text-align: left; vertical-align: top;">or       </td><td style="text-align: left; vertical-align: top;">(n2 n1) &#8594; (результат)&nbsp;</td><td style="text-align: left; vertical-align: top;">n2 ИЛИ n1</td></tr>
</table>

<p> Эти операции также должны давать правильный результат, если в одном или
обеих операндах &laquo;истина&raquo; представлена любым ненулевым целым числом.

<p><b>Операции со стеком</b>

<p>При выполнении вычислений на стеке часто возникает необходимость 
изменять порядок следования элементов, удалять значения,
копировать их и т.д. Для этого реализуйте следующие операции:
<table>
<tr><td style="text-align: left; vertical-align: top;">drop       </td><td style="text-align: left; vertical-align: top;">(n1)       &#8594; () &nbsp;       </td><td style="text-align: left; vertical-align: top;">Удаляет элемент на вершине стека</td></tr>
<tr><td style="text-align: left; vertical-align: top;">swap       </td><td style="text-align: left; vertical-align: top;">(n2 n1)    &#8594; (n1 n2)         </td><td style="text-align: left; vertical-align: top;">Меняет местами два элемента на вершине стека</td></tr>
<tr><td style="text-align: left; vertical-align: top;">dup        </td><td style="text-align: left; vertical-align: top;">(n1)       &#8594; (n1 n1)         </td><td style="text-align: left; vertical-align: top;">Дублирует элемент на вершине стека</td></tr>
<tr><td style="text-align: left; vertical-align: top;">over       </td><td style="text-align: left; vertical-align: top;">(n2 n1)    &#8594; (n1 n2 n1)&nbsp;</td><td style="text-align: left; vertical-align: top;">Копирует предпоследний элемент на вершину стека</td></tr>
<tr><td style="text-align: left; vertical-align: top;">rot        </td><td style="text-align: left; vertical-align: top;">(n3 n2 n1) &#8594; (n1 n2 n3)&nbsp;</td><td style="text-align: left; vertical-align: top;">Меняет местами первый и третий элемент от головы стека</td></tr>
<tr><td style="text-align: left; vertical-align: top;">depth&nbsp;</td><td style="text-align: left; vertical-align: top;">(...)      &#8594; (n ...)         </td><td style="text-align: left; vertical-align: top;">Возвращает число элементов в стеке перед своим вызовом</td></tr>
</table>

<p><b>Управляющие конструкции</b>
<table>
<tr><td style="text-align: left; vertical-align: top;">define <i>word</i>&nbsp;</td><td style="text-align: left; vertical-align: top;">() &#8594; ()          </td><td style="text-align: left; vertical-align: top;">Начинает словарную статью &mdash; определение слова <i>word</i></td></tr>
<tr><td style="text-align: left; vertical-align: top;">end                     </td><td style="text-align: left; vertical-align: top;">() &#8594; ()          </td><td style="text-align: left; vertical-align: top;">Завершает статью</td></tr>
<tr><td style="text-align: left; vertical-align: top;">exit                    </td><td style="text-align: left; vertical-align: top;">() &#8594; ()          </td><td style="text-align: left; vertical-align: top;">Завершает выполнение процедуры (кода статьи)</td></tr>
<tr><td style="text-align: left; vertical-align: top;">if                      </td><td style="text-align: left; vertical-align: top;">(флаг) &#8594; ()&nbsp;</td><td style="text-align: left; vertical-align: top;">Если флаг не равен 0, то выполняется код в теле if..endif, иначе выполнение кода до endif пропускается</td></tr>
<tr><td style="text-align: left; vertical-align: top;">endif                   </td><td style="text-align: left; vertical-align: top;">() &#8594; ()          </td><td style="text-align: left; vertical-align: top;">Завершает тело if</td></tr>
</table>
<p>Пусть слово define&nbsp;<i>word</i> начинает определение слова <i>word</i>. 
В теле определения
(словарной статьи) следуют слова, которые надо вычислить, чтобы вычислить слово 
word. Статья заканчивается словом end. Определенное таким образом слово 
может быть использовано в программе так же, как и встроенное. Например,
унарный декремент может быть определен, а затем использован так:
<pre>
(interpret #(   <span style="color: blue;">define -- 1 - end
                5 -- --</span>      ) '())
  &#8658; (3)
</pre>
<p>Завершить выполнение процедуры до достижения её окончания end можно с помощью 
слова exit.
<p>В статьях допускаются рекурсивные определения.
Вложенные словарные статьи не допускаются. 
<p>Конструкции if...endif не должны быть вложенными (в ЛР). В программах ниже
даны примеры их использования.

<h2>Примеры программ</h2>
Ниже представлены программы, которые будут выполнены сервером тестирования
с помощью вашего интерпретатора (наряду с более короткими примерами).
<pre>
(interpret #(   <span style="color: blue;">define abs 
                    dup 0 &lt; 
                    if neg endif 
                end 
                 9 abs 
                -9 abs</span>      ) (quote ()))
  &#8658; (9 9)

(interpret #(   <span style="color: blue;">define =0? dup 0 = end 
                define &lt;0? dup 0 &lt; end 
                define signum 
                    =0? if exit endif 
                    &lt;0? if drop -1 exit endif 
                    drop 
                    1 
                end 
                 0 signum 
                -5 signum 
                10 signum</span>       ) (quote ()))
  &#8658; (1 -1 0)

(interpret #(   <span style="color: blue;">define -- 1 - end 
                define =0? dup 0 = end 
                define =1? dup 1 = end 
                define factorial 
                    =0? if drop 1 exit endif 
                    =1? if drop 1 exit endif 
                    dup -- 
                    factorial 
                    * 
                end 
                0 factorial 
                1 factorial 
                2 factorial 
                3 factorial 
                4 factorial</span>     ) (quote ()))
  &#8658; (24 6 2 1 1)

(interpret #(   <span style="color: blue;">define =0? dup 0 = end 
                define =1? dup 1 = end 
                define -- 1 - end 
                define fib 
                    =0? if drop 0 exit endif 
                    =1? if drop 1 exit endif 
                    -- dup 
                    -- fib 
                    swap fib 
                    + 
                end 
                define make-fib 
                    dup 0 &lt; if drop exit endif 
                    dup fib 
                    swap -- 
                    make-fib 
                end 
                10 make-fib</span>     ) (quote ()))
  &#8658; (0 1 1 2 3 5 8 13 21 34 55)

(interpret #(   <span style="color: blue;">define =0? dup 0 = end 
                define gcd 
                    =0? if drop exit endif 
                    swap over mod 
                    gcd 
                end 
                90 99 gcd 
                234 8100 gcd</span>    ) '())
  &#8658; (18 9)
</pre>

<h2>Рекомендации</h2>
<p>В составе интерпретатора определите главную процедуру, которая будет обрабатывать
каждое слово программы. Пусть состояние интерпретатора описывают аргументы этой процедуры:
вектор слов, счетчик слов (индекс текущего слова), стек данных, стек возвратов и словарь (ассоциативный
список). 
<p>Главная процедура классифицирует слово, на которое указывает счетчик, и интерпретирует его как
число или слово (определенное в программе или встроенное). Встроенные слова принимают состояние
интерпретатора и возвращают его измененным согласно семантике слова. 
<p>Изменяться могут счетчик, стек данных, стек возвратов и словарь. Не храните ни их, 
ни интерпретируемую программу
в глобальных или статических переменных (почему?).
<p>Если в программе встречается
определение статьи, то в словарь помещается новое слово (ключ) и индекс первого слова в статье (значение).
<p>При вызове такой статьи в стек возвратов помещается индекс слова, следующего за вызовом. Он будет снят
с вершины стека и возвращен в качестве значения счетчика слов при возврате из статьи (слова end и exit).
Такой подход позволяет интерпретировать вложенные и рекурсивные вызовы. Также в коде интерпретатора
целесообразно определить
словарь соответствий слов исходного языка встроенным процедурам интерпретатора.
<p>При необходимости организуйте отложенные вычисления.
В процессе разработки используйте юнит-тестирование.
