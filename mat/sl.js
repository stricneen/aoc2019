import {MDCSlider} from '@material/slider';

const slider = new MDCSlider(document.querySelector('.mdc-slider'));
slider.listen('MDCSlider:change', () => console.log(`Value changed to ${slider.value}`));mdc.autoInit();
