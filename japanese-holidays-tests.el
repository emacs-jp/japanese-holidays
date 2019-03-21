(require 'japanese-holidays)

(ert-deftest japanese-holiday-test-2019/04/30 ()
  "Check if 2019/04/30 is \"国民の休日\"."
  (let ((calendar-holidays japanese-holidays))
    (should (equal (calendar-check-holidays '(4 30 2019)) '("国民の休日")))))

(ert-deftest japanese-holiday-test-2019/05/01 ()
  "Check if 2019/05/01 is \"即位の日\", and not in other years."
  (let ((calendar-holidays japanese-holidays))
    (should (not (calendar-check-holidays '(5 1 2018))))
    (should (equal (calendar-check-holidays '(5 1 2019)) '("即位の日")))
    (should (not (calendar-check-holidays '(5 1 2020))))))

(ert-deftest japanese-holiday-test-2019/05/02 ()
  "Check if 2019/05/02 is \"国民の休日\"."
  (let ((calendar-holidays japanese-holidays))
    (should (equal (calendar-check-holidays '(5 2 2019)) '("国民の休日")))))

(ert-deftest japanese-holiday-test-2019/10/22 ()
  "Check if 2019/10/22 is \"即位礼正殿の儀\", and not in other years."
  (let ((calendar-holidays japanese-holidays))
    (should (not (calendar-check-holidays '(10 22 2018))))
    (should (equal (calendar-check-holidays '(10 22 2019)) '("即位礼正殿の儀")))
    (should (not (calendar-check-holidays '(10 22 2020))))))

(ert-deftest japanese-holiday-test-emperor-birthday-change-at-2019 ()
  "Check if 2018/12/23 and 2020/02/23 are \"天皇誕生日\"."
  (let ((calendar-holidays japanese-holidays))
    (should (equal (calendar-check-holidays '(12 23 2018)) '("天皇誕生日")))
    (should (equal (calendar-check-holidays '(2 23 2020)) '("天皇誕生日")))
    (should (not (calendar-check-holidays '(2 23 2019))))
    (should (not (calendar-check-holidays '(12 23 2019))))))

(ert-deftest japanese-holiday-test-2020/07/23 ()
  "Check if 2020/07/23 is \"海の日\", and not in other years."
  (let ((calendar-holidays japanese-holidays))
    (should (not (calendar-check-holidays '(7 23 2019))))
    (should (equal (calendar-check-holidays '(7 15 2019)) '("海の日")))
    (should (not (calendar-check-holidays '(7 20 2020))))
    (should (equal (calendar-check-holidays '(7 23 2020)) '("海の日")))
    (should (not (calendar-check-holidays '(7 23 2021))))
    (should (equal (calendar-check-holidays '(7 19 2021)) '("海の日")))))

(ert-deftest japanese-holiday-test-2020/07/24 ()
  "Check if 2020/07/24 is \"スポーツの日\", and not in other years."
  (let ((calendar-holidays japanese-holidays))
    (should (not (calendar-check-holidays '(7 24 2019))))
    (should (equal (calendar-check-holidays '(10 14 2019)) '("体育の日")))
    (should (not (calendar-check-holidays '(10 12 2020))))
    (should (equal (calendar-check-holidays '(7 24 2020)) '("スポーツの日")))
    (should (not (calendar-check-holidays '(7 24 2021))))
    (should (equal (calendar-check-holidays '(10 11 2021)) '("スポーツの日")))))

(ert-deftest japanese-holiday-test-2020/08/10 ()
  "Check if 2020/08/10 is \"山の日\", and not in other years."
  (let ((calendar-holidays japanese-holidays))
    (should (not (calendar-check-holidays '(8 10 2019))))
    (should (equal (calendar-check-holidays '(8 11 2019)) '("山の日")))
    (should (not (calendar-check-holidays '(8 11 2020))))
    (should (equal (calendar-check-holidays '(8 10 2020)) '("山の日")))
    (should (not (calendar-check-holidays '(8 10 2021))))
    (should (equal (calendar-check-holidays '(8 11 2021)) '("山の日")))))
