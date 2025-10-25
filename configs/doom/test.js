function playSound(filename) {
  try {
    const audio = new Audio(`resourcdes/${filename}`);
    audio.volume = 0.5;
    audio.play().catch(()=> {}));
  } catch (e) {
    //ignore audio erros
  }
}
