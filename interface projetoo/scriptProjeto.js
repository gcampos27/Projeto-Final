document.getElementById("prediction-form").addEventListener("submit", function(event) {
    event.preventDefault();

    // Captura os valores do formulário
    let age = document.getElementById("age").value;
    let studyTime = document.getElementById("study-time").value;
    let internet = document.getElementById("internet").value;

    // Simulação da previsão (substituir por integração com API)
    let prediction = simulatePrediction(age, studyTime, internet);

    // Exibe o resultado
    document.getElementById("result").innerText = `Previsão de Adaptabilidade: ${prediction}`;
});

// Simulação de previsão (isso seria substituído pela API real)
function simulatePrediction(age, studyTime, internet) {
    let score = parseInt(age) + parseInt(studyTime) + parseInt(internet);
    
    if (score > 30) return "Alta";
    else if (score > 20) return "Média";
    else return "Baixa";
}
