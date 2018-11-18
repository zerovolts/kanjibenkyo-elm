use crate::models::kana;

pub struct KanaQuiz {
    pub correct_answer_choice: (Kana, KanaCategory),
    pub answer_choices: (Vec<Kana>, KanaCategory),
}

impl KanaQuiz {
    pub fn new() -> KanaQuiz {
        KanaQuiz {
            correct_answer_choice: (Kana)
        }
    }
}