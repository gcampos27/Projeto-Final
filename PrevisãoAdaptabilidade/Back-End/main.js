document.getElementById("predict-form").addEventListener("submit", function (e) {
  e.preventDefault();

  const formData = {
    Gender: document.getElementById("Gender").value,
    Age: parseInt(document.getElementById("Age").value),
    "Education.Level": document.getElementById("Education_Level").value,
    "Institution.Type": document.getElementById("Institution_Type").value,
    "IT.Student": document.getElementById("IT_Student").value,
    Location: document.getElementById("Location").value,
    Load_shedding: document.getElementById("Load_shedding").value,
    "Self.Lms": document.getElementById("Self_Lms").value,
    "Financial.Condition": document.getElementById("Financial_Condition").value,
    "Internet.Type": document.getElementById("Internet_Type").value,
    "Network.Type": document.getElementById("Network_Type").value,
    "Class.Duration": document.getElementById("Class_Duration").value,
    Device: document.getElementById("Device").value
  };

  const displayData = {
    Gender: document.getElementById("Gender").selectedOptions[0].text,
    Age: document.getElementById("Age").selectedOptions[0].text,
    "Education.Level": document.getElementById("Education_Level").selectedOptions[0].text,
    "Institution.Type": document.getElementById("Institution_Type").selectedOptions[0].text,
    "IT.Student": document.getElementById("IT_Student").selectedOptions[0].text,
    Location: document.getElementById("Location").selectedOptions[0].text,
    "Load.shedding": document.getElementById("Load_shedding").selectedOptions[0].text,
    "Self.Lms": document.getElementById("Self_Lms").selectedOptions[0].text,
    "Financial.Condition": document.getElementById("Financial_Condition").selectedOptions[0].text,
    "Internet.Type": document.getElementById("Internet_Type").selectedOptions[0].text,
    "Network.Type": document.getElementById("Network_Type").selectedOptions[0].text,
    "Class.Duration": document.getElementById("Class_Duration").selectedOptions[0].text,
    Device: document.getElementById("Device").selectedOptions[0].text
  };

  // Validação rápida
  console.log("Dados a enviar:", formData);

  // Enviar para o Plumber
  fetch("http://localhost:8000/predict", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(formData)
  })
  .then(res => res.json())
  .then(data => {
    // data.predito deve conter a previsão do modelo
    if(data.predito !== undefined){
      alert(`A previsão é: ${data.predito}`);
    } else {
      alert('Previsão não recebida corretamente.');
      console.log('Resposta completa:', data);
    }
  })
  .catch(err => {
    console.error('Erro:', err);
    alert('Erro ao tentar fazer a previsão.');
  });
});