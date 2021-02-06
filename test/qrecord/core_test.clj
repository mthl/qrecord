(ns qrecord.core-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [qrecord.core :as q]))

(q/defrecord Foo [foo/x foo/y])

(deftest qrecord-test
  (let [qr (->Foo 7 "z")]
    (testing "lookup"
      (is (= 7 (:foo/x qr)))
      (is (= "z" (:foo/y qr))))

    (testing "count"
      (is (= 2 (count qr))))

    (testing "seq"
      (is (= [[:foo/x 7] [:foo/y "z"]]
             (seq qr))))

    (testing "assoc"
      (is (= 4 (:foo/x (assoc qr :foo/x 4)))))

    (testing "iterator"
      (is (= [[:foo/x 7] [:foo/y "z"]]
             (iterator-seq (.iterator qr)))))

    (testing "dissoc"
      (is (not (instance? Foo (dissoc qr :foo/x))))
      (is (instance? Foo (dissoc qr :x))))))
