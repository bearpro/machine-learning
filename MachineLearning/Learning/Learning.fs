namespace MachineLearning.Learning

/// <summary>
/// Представляет отдельную запись в таблице значений обучающего набора.
/// </summary>
type LearningTarget =
    { Inputs: float list
      Output: float }

/// <summary>
/// Представляет таблицу значений для входов и выхода нейрона, по которой нейрон
/// будет обучаться.
/// </summary>
type LearningTable = LearningTarget list

/// <summary>
/// Представляет результат проверки соответствия.
/// </summary>
type CorrectionCheckResult =
    | Error of LearningTarget
    | Correct
