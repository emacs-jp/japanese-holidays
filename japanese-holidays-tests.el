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
