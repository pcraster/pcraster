import performance_analyst as pa
import performance.timer_data


class TimerCase(pa.TimerCase):

    data = performance.timer_data.TimerData()

    repeat = 3
