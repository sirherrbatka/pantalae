(cl:in-package #:pantalea.utils.bloom-filter)


(defvar *hashes* #((3363490446 4086217716)
                   (404370446 1176441999)
                   (3234610116 3439750920)
                   (2575700989 414604988)
                   (171381825 1774744635)
                   (556341779 237968744)
                   (3643402538 1941599746)
                   (2083120894 2767525426)
                   (4077394319 4272525031)
                   (1451916007 1558711357)
                   (3114187492 1709549131)
                   (737028909 3085360311)
                   (3590238452 274798275)
                   (2854116039 723272092)
                   (486201859 655297946)
                   (1698731658 1702847708)
                   (423242331 3934266541)
                   (2002816340 3132445400)
                   (3162414256 2819853427)
                   (926867968 221159232)
                   (252108314 3480686231)
                   (1360727791 3797144724)
                   (1757106006 109520212)
                   (1414811768 1587333052)
                   (398188340 1232254610)
                   (2119746089 289379017)
                   (1610718606 1811185798)
                   (3732165768 4282260204)
                   (184820093 3678705335)
                   (1084867034 1200634140)
                   (1361342998 3336073388)
                   (822428141 4240348943)
                   (2739986314 2667408852)
                   (3782783512 3094116720)
                   (1995269918 4071342218)
                   (3973274792 2732854883)
                   (1468745824 1161523326)
                   (2758572096 1843630982)
                   (1426717538 1033606524)
                   (2593461473 1709343735)
                   (1696457266 3316035838)
                   (2613301819 3346024089)
                   (3567962110 1072303166)
                   (550663884 4203427051)
                   (959491495 1927186295)
                   (2886471107 3905695311)
                   (3602455956 3770515788)
                   (4169182347 1816382062)
                   (3596074776 2892345381)
                   (2995703620 346761085)
                   (891164654 2229872005)
                   (3944817853 2494900868)
                   (25827968 42605297)
                   (852004784 1689563162)
                   (3587407156 1251638867)
                   (3956400144 968164509)
                   (363499220 2623728343)
                   (1494235405 4294371569)
                   (3878198507 553215323)
                   (2644918482 4242701100)
                   (2772910691 3252887615)
                   (2863214945 2319400461)
                   (3136420083 3348965492)
                   (2550174062 151050341)
                   (1598635541 334635542)
                   (2049214435 1210292261)
                   (71222138 2009087692)
                   (457448718 881146725)
                   (3664204417 4082855445)
                   (3663329917 4210357809)
                   (3527486802 2569385804)
                   (2309000765 3921900019)
                   (3678640076 3613685474)
                   (3385237272 3154857385)
                   (2518841064 2419053996)
                   (3969548485 3151733449)
                   (1249681633 1368483368)
                   (238034273 4208701947)
                   (3403832725 1917044074)
                   (4230215671 987712840)
                   (3284119632 1770310834)
                   (2272149256 2295733216)
                   (2029061726 1420837132)
                   (2836854973 273776580)
                   (426281554 1685861555)
                   (2859028255 1985959120)
                   (1758801207 462673225)
                   (1248385476 1112519953)
                   (579473511 2592713719)
                   (3926299538 1040891653)
                   (3874351211 1903244241)
                   (2416701999 1508122208)
                   (157531184 2421430989)
                   (1491165735 3314520194)
                   (3833725167 3702234087)
                   (1450032843 2939309820)
                   (1676319387 2697422981)
                   (3966879770 1184851100)
                   (2770295671 1796104491)
                   (4260805313 2452868627)
                   (1882665915 3931887505)
                   (1201324195 477880684)
                   (3267939600 1241189548)
                   (1131098882 2089150983)
                   (3999906914 191327779)
                   (4193928045 3362940567)
                   (918962491 442802528)
                   (2868758588 2777887450)
                   (1731268225 2644214523)
                   (1901574407 1139716314)
                   (239252818 3038632949)
                   (3438832447 4154544779)
                   (269811791 1638485324)
                   (548548888 1886180225)
                   (3753815616 1729326608)
                   (966149928 2367813487)
                   (3496717750 2668456969)
                   (1334928841 983963396)
                   (3702554615 2929647161)
                   (2479117918 717644749)
                   (2738017269 3856083492)
                   (3060080942 2266271948)
                   (981783225 4079824256)
                   (1709741047 120392511)
                   (1920136593 594985212)
                   (2207588836 3462905334)
                   (3936879052 3541348965)
                   (3880905942 1196395351)
                   (3540091170 3078765390)
                   (3353858245 3224691896)
                   (2997051077 3390183362)
                   (3115470761 116426970)
                   (4093525620 585691733)
                   (561461718 2986534572)
                   (3521019126 3148717547)
                   (2435052651 2257780991)
                   (3295104125 2912446382)
                   (3325599782 1478864177)
                   (2045842834 2464830712)
                   (1171364610 682715999)
                   (597636689 458256856)
                   (1264581215 17273196)
                   (593370471 741107679)
                   (2795583025 3452336576)
                   (2229449623 380183779)
                   (1955734716 2401467298)
                   (1071806042 439558162)
                   (1795448588 3645616041)
                   (4286553781 4120218221)
                   (201952915 4183543706)
                   (2969539594 2896888904)
                   (862479327 3717454735)
                   (86621537 1609114124)
                   (1345445937 3070648776)
                   (1420721292 1413795107)
                   (2643175200 1348222327)
                   (2879553945 3134392672)
                   (2746553771 2732453903)
                   (438129946 492730903)
                   (2388344532 3193756469)
                   (3423367096 2343498361)
                   (3157301260 3392902953)
                   (1707178094 3904565878)
                   (297881982 384530297)
                   (4110041206 3674648725)
                   (3531453113 2027376578)
                   (2888809227 704400727)
                   (3289703114 1037627351)
                   (2307943701 3105141941)
                   (620527307 1791638453)
                   (1700222599 2681320752)
                   (1293805339 3015879164)
                   (1662174365 1363900744)
                   (578031777 1870290001)
                   (40250331 4283270024)
                   (4168140720 3382580702)
                   (4201204528 2163423368)
                   (509670989 3644480339)
                   (348953150 3689494100)
                   (831806170 2638473230)
                   (108384759 525129562)
                   (3201253705 495200818)
                   (2148897017 3678193481)
                   (1426705769 2263653682)
                   (1913529155 3303545893)
                   (1775731537 3992723360)
                   (1881136494 3871613901)
                   (3842828347 1973730051)
                   (1449022603 1816723564)
                   (935486731 1066247998)
                   (43442787 3905131145)
                   (318722469 137969335)
                   (3922653459 3394781897)
                   (1495144294 2950896548)
                   (2523549782 2427959932)
                   (3204284337 1186049286)
                   (4015601922 3468396689)
                   (2194659630 1790891005)
                   (1230733199 3049954710)
                   (2684474999 1477946054)
                   (1909772279 777712915)
                   (2580037764 1805877811)
                   (3231315390 1906819045)
                   (3741192016 2489596315)
                   (1336558484 3326835115)
                   (2397455378 951616564)
                   (468091582 4109050590)
                   (1996647809 958595957)
                   (3874299567 9504786)
                   (105677905 438506082)
                   (2650521039 1095807452)
                   (1451658948 3097977779)
                   (463290187 1930769438)
                   (3261547513 1036791209)
                   (3151035991 1170285938)
                   (3818274170 3339536836)
                   (1968372805 3086852652)
                   (1186603070 70669832)
                   (1638569325 1455582985)
                   (2402125270 1575314013)
                   (3383365517 2439753462)
                   (314184314 4020770502)
                   (2273911153 2740063266)
                   (2552512123 4264502803)
                   (3544737912 2641105602)
                   (2236096342 3094295258)
                   (2898050587 2861896595)
                   (2030754906 246325017)
                   (2811469883 4108972367)
                   (4103704226 3891903368)
                   (2929877159 3539009164)
                   (18293766 2924999677)
                   (3183436707 3942948478)
                   (2492102703 3595635232)
                   (1353651249 2069576326)
                   (723606553 1611064790)
                   (3723533933 1765132094)
                   (2921135691 1269385037)
                   (2964988625 326298089)
                   (2537320202 2075689920)
                   (3471190680 4148280726)
                   (2998794261 464938132)
                   (1821789370 359039185)
                   (2379839740 2761187869)
                   (2593569871 3113400158)
                   (979453306 1651070055)
                   (1449076390 4063755770)
                   (3464987033 2310908165)
                   (3468813888 503947210)
                   (1624803534 3171469972)
                   (3413509398 152778962)
                   (310002531 1591439016)
                   (3401059998 1424012493)
                   (2699922467 2283218712)
                   (40293410 2261381514)
                   (1806374301 3568420276)
                   (2034253195 3669352834)
                   (1595801471 2146301246)
                   (2596810780 2372835596)
                   (1831738775 2207504167)
                   (2190713265 722287139)
                   (3433373318 3578901879)
                   (621700235 372364134)
                   (3193050429 3745421913)
                   (3533792022 1193916378)
                   (2314992091 3344539005)
                   (1404646616 3962211850)
                   (3649849173 230735732)
                   (3976257313 143297888)
                   (4134793369 2873860186)
                   (330405232 2178025230)
                   (3535293421 332129148)
                   (1740904164 1320315490)
                   (1943868362 1775902309)
                   (335674544 716060172)
                   (2246457389 1253728860)
                   (4240793739 2578459915)
                   (1900744476 2563321948)
                   (1553950435 938413725)
                   (3061142696 424329571)
                   (1872350223 1984517103)
                   (4017428397 570105749)
                   (579273058 2337057693)
                   (4142473853 3335048275)
                   (827939154 1076978789)
                   (3847641541 4111150675)
                   (1636457524 2674837390)
                   (782159573 776040423)
                   (688891618 3180950192)
                   (687782534 1679964477)
                   (2738336825 272855576)
                   (1581144126 1693951845)
                   (2978902194 3075520766)
                   (2749313733 349245502)
                   (1874146183 2787873697)
                   (1353028317 2629101256)
                   (3943684347 2926618313)
                   (2881600984 2792441416)
                   (2896243819 1399747019)
                   (4064420661 1932472852)
                   (4002343556 115259491)
                   (2785586806 2796532337)
                   (4019162590 555064786)
                   (419018538 2830468787)
                   (3041119164 609983082)
                   (2536667506 4273019476)
                   (93856630 4204671851)
                   (2369278309 2923342051)
                   (1885880163 3274173931)
                   (3334697065 92244728)
                   (1196730083 3001421795)
                   (1712705042 3624113458)
                   (3380471557 2409046702)
                   (1026191228 1284624943)
                   (606885411 161234343)
                   (1095654063 3042101804)
                   (609081371 1185846656)
                   (2614952463 1223079049)
                   (2225489337 660451857)
                   (3554881735 2822055016)
                   (1119554776 3551807758)
                   (1459509196 1141309060)
                   (1224681031 2547349531)
                   (930600187 2879962111)
                   (1599508621 4200181833)
                   (3784923510 1479115092)
                   (1715568034 1579103843)
                   (151342426 1058577411)
                   (2192860282 3263983952)
                   (1230198515 824346422)
                   (757224615 1179843786)
                   (4255319782 3585348844)
                   (3347081960 368413676)
                   (3970376738 2737649035)
                   (1397648423 1499525956)
                   (587611745 195690614)
                   (2004610889 2689211891)
                   (3604535296 3929972522)
                   (1399242526 3088407631)
                   (3934651903 1386026765)
                   (2114561911 2063776030)
                   (3569386648 958560479)
                   (2977661876 3484318978)
                   (553042653 1889542857)
                   (1188243734 2592475956)
                   (405813978 430549136)
                   (1215975887 2601488966)
                   (3006457624 2929320506)
                   (2286517531 1233666122)
                   (2697675 2665803324)
                   (2210054591 2801479353)
                   (2821835380 3238947696)
                   (686311413 151660038)
                   (237654148 1395812589)
                   (1338155693 2802833609)
                   (4194717925 2234408174)
                   (1206554025 2578961309)
                   (1538145243 923797659)
                   (1488528899 789495949)
                   (1543114481 2050902067)
                   (1337862188 3386659863)
                   (4116323911 346758228)
                   (4094271343 3135335463)
                   (1077003968 1902586361)
                   (960224934 4133901877)
                   (1097566471 3492749477)
                   (3291888765 4204683957)
                   (1162023844 663478404)
                   (387951883 3365242650)
                   (492532156 532996711)
                   (2366878139 2351844505)
                   (419191056 1407993071)
                   (4097232873 287471442)
                   (2298526393 478113897)
                   (723733798 3707741573)
                   (1780406926 1968510495)
                   (3773462331 3307743815)
                   (1884612197 1524948889)
                   (458347066 2583701940)
                   (1222406884 77712651)
                   (3773698329 3789317723)
                   (4156170416 905192820)
                   (2305686414 2583657764)
                   (3014889848 4096439241)
                   (2127645356 3202543611)
                   (2406857046 1084825589)
                   (3955791432 264436337)
                   (3853187407 3596088854)
                   (106675123 4251329608)
                   (3736145591 970818599)
                   (2311379508 2031550691)
                   (1584951025 3312357739)
                   (2917504256 220857550)
                   (2244289150 2642071963)
                   (4115793619 1363138228)
                   (1189640965 2379176226)
                   (2363113360 1881108460)
                   (2651248796 508682106)
                   (2079216306 3567954541)
                   (2739922268 2497827999)
                   (577268131 2865028790)
                   (1825462178 997804738)
                   (3725300671 3968046994)
                   (2640525980 1415789469)
                   (1395744635 3271786154)
                   (2060545334 141652380)
                   (433967870 1601436731)
                   (2271914742 2930329007)
                   (1100310668 434149393)
                   (1400899691 570232921)
                   (3861009981 768251542)
                   (124090420 2979770918)
                   (1415047029 1428546568)
                   (4211665633 1527101308)
                   (3674640972 3660382256)
                   (115498467 3820703862)
                   (921271065 2671627138)
                   (693194206 3012282683)
                   (211338800 3548852272)
                   (3173831884 3372201736)
                   (2588621609 4177227927)
                   (2571008109 1721643267)
                   (992966576 1250102554)
                   (1566977547 2733910504)
                   (1345375193 1763704995)
                   (2896389244 3916756276)
                   (2638882607 2342793709)
                   (758814232 4123546062)
                   (3733828361 359046114)
                   (2572729226 1724033108)
                   (745804679 1320904721)
                   (3335419184 3677485767)
                   (3267182696 2419292619)
                   (3544744848 974999278)
                   (1290348401 276900107)
                   (1595486549 4027763770)
                   (2105780332 730125637)
                   (949159371 3685828907)
                   (3599556893 1940706935)
                   (1639728593 993191411)
                   (1525827944 967493702)
                   (3468759490 3599632907)
                   (2776786652 3765303330)
                   (1647267730 1039042602)
                   (3228166888 654516857)
                   (4099774740 3082249575)
                   (410209796 2546630001)
                   (2864665524 3130582261)
                   (3719934632 2368557457)
                   (848058201 1949250944)
                   (1283372362 794172702)
                   (2640564785 2252660330)
                   (3911578181 582727528)
                   (2784288874 248723197)
                   (4241673839 3958216011)
                   (186549741 553198631)
                   (3523734992 2389854207)
                   (1846102792 3404247876)
                   (3216075104 456016676)
                   (792168264 1007229102)
                   (1921662656 2834384682)
                   (1412595935 1445808087)
                   (2040772439 2802944456)
                   (722184666 1164797906)
                   (2877098666 2270170721)
                   (437589054 2804921297)
                   (3810757530 2579239042)
                   (2285145815 2565821297)
                   (1324171015 1860716086)
                   (2590016351 83020443)
                   (2559442673 935658414)
                   (223854198 1620251131)
                   (3971328147 286776617)
                   (2970666690 1694233198)
                   (814684695 3899354093)
                   (1558653497 873011879)
                   (3455057369 3177987384)
                   (867407693 689048646)
                   (2344280345 1836899313)
                   (2519000177 2044324637)
                   (4130751843 4058035748)
                   (2748806571 1369307126)
                   (866649332 1667147627)
                   (4129286344 1735588241)
                   (3327939961 3691167149)
                   (4176527662 34979984)
                   (2549266763 3874350103)
                   (1306590495 20990073)
                   (2712978747 2530473068)
                   (3415481491 147785408)
                   (3741441766 123510378)
                   (4268803056 1336766116)
                   (2697829444 1660224331)
                   (4265736896 2549589655)
                   (1113563142 1106808506)
                   (3630392356 58411193)
                   (521372862 980081840)
                   (2545217633 2150801861)
                   (2286657258 2774100876)
                   (2525919774 661065860)
                   (3743632701 2517167419)
                   (370056006 4110086039)
                   (2784864482 1031744782)
                   (2384236966 4154246043)
                   (1438380592 2398165897)
                   (491351460 758377983)
                   (1801416099 4011369227)
                   (2380968563 483454477)
                   (2814270549 888352279)
                   (4093287712 635863374)
                   (4221310820 3318487706)
                   (2882555515 935511372)))

(defvar *depth* 512)

(defvar *width* 16)

(eval-always
  (define-constant +long-prime+ 4294967311)
  (define-constant +max-64-bits+ #xFFFFFFFFFFFFFFFF))

(defun make-sketch ()
  (make-array *depth*
              :initial-element 0
              :element-type 'non-negative-fixnum))

(defun jaccard (a-counters b-counters)
  (coerce (/ (- (array-total-size a-counters)
                  (iterate
                    (for i from 0 below (array-total-size a-counters))
                    (counting (= 0 (row-major-aref a-counters i) (row-major-aref b-counters i)))))
               (array-total-size a-counters))
            'single-float))

(defun hashval-no-depth (hashes j hash)
  (declare (type non-negative-fixnum j hash))
  (~> (aref hashes j 0)
      (* hash)
      (ldb (byte 32 0) _)
      (+ (aref hashes j 1))
      (ldb (byte 32 0) _)
      (rem +long-prime+)))

(defun hashval (hashes depth j hash)
  (declare (type non-negative-fixnum depth j hash))
  (~> (hashval-no-depth hashes j hash)
      (rem depth)))

(defun add-hash! (counters hash)
  (setf hash (ldb (byte 32 0) hash))
  (iterate
    (for j from 0 below *width*)
    (setf (aref counters (hashval *hashes* *depth* j hash)) 1)))

(defun add-key! (sketch key)
  (add-hash! sketch (pantalea.utils.hashing:hash-key key)))
